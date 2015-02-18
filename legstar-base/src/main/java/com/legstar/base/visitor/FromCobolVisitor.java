package com.legstar.base.visitor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolOptionalType;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;
import com.legstar.base.type.primitive.FromHostPrimitiveResult;
import com.legstar.base.utils.StringUtils;

/**
 * Generic visitor using mainframe data as input and producing some kind of
 * output (up to the specialized classes).
 * <p/>
 * Mainframe data is expected as a byte array.
 * 
 */
public abstract class FromCobolVisitor implements CobolVisitor {

    /**
     * Host COBOL configuration parameters.
     */
    private final CobolContext cobolContext;

    /**
     * Incoming mainframe data.
     */
    private final byte[] hostData;

    /**
     * Where to start processing in the incoming mainframe data buffer.
     */
    private final int start;

    /**
     * Certain field values might need to be accessed, after they were visited,
     * by some other node downstream.
     * <p/>
     * One use case is variable size arrays which dimension is given at runtime
     * by a numeric field marked as odoObject.
     * <p/>
     * Another use case is custom code invoked by this visitor that might need
     * field values to make decisions. Such fields are marked as customVariable.
     */
    private final Map < String, Object > variables;

    /**
     * There are 2 mechanisms to mark custom variables:
     * <ul>
     * <li>From within a primitive type description using the customVariable
     * property</li>
     * <li>At runtime by specifying the custom variable name in this collection</li>
     * </ul>
     * TODO custom variable names are not namespaced within a structure and
     * there is a conflict risk
     */
    private final Set < String > customVariables;

    /**
     * By default, this is the strategy for choice alternative selection.
     */
    private final FromCobolChoiceStrategy defaultChoiceStrategy;

    /**
     * Optionally, user might provide a custom strategy for alternative
     * selection.
     */
    private final FromCobolChoiceStrategy customChoiceStrategy;

    /**
     * Field being visited. The name identifies this field to its immediate
     * parent.
     */
    private String curFieldName;

    /**
     * After the visitor is done visiting, this will contain the last position
     * in the incoming mainframe data buffer that was processed.
     */
    private int lastPos;

    /**
     * Items following choices must start at fixed positions even if the chosen
     * alternative is shorter than the largest one. This situation results in an
     * extra offset that will be used to position such items.
     */
    private int extraOffset;

    /**
     * Current COBOL type name visited relative to its parents.
     */
    private Stack < String > cobolNamesStack;

    // -----------------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------------
    public FromCobolVisitor(CobolContext cobolContext, byte[] hostData,
            int start) {
        this(cobolContext, hostData, start, null);
    }

    public FromCobolVisitor(CobolContext cobolContext, byte[] hostData,
            int start, FromCobolChoiceStrategy customChoiceStrategy) {
        this(cobolContext, hostData, start, customChoiceStrategy, null);
    }

    public FromCobolVisitor(CobolContext cobolContext, byte[] hostData,
            int start, FromCobolChoiceStrategy customChoiceStrategy,
            Set < String > customVariables) {
        if (hostData == null) {
            throw new IllegalArgumentException("Mainframe data buffer is null");
        }
        if (start >= hostData.length) {
            throw new IllegalArgumentException("Start position " + start
                    + " is outside the mainframe data buffer");
        }
        this.cobolContext = cobolContext;
        this.hostData = hostData;
        this.start = start;
        this.lastPos = start;
        this.variables = new HashMap < String, Object >();
        this.customVariables = customVariables;
        this.customChoiceStrategy = customChoiceStrategy;
        this.defaultChoiceStrategy = new DefaultFromCobolChoiceStrategy(
                cobolContext);
        this.cobolNamesStack = new Stack < String >();
    }

    // -----------------------------------------------------------------------------
    // Visiting logic available to specific implementations
    // -----------------------------------------------------------------------------
    /**
     * Visit a complex type which visits each of its children in turn.
     * <p/>
     * Optional children may be skipped.
     * 
     * @param type the complex type to visit
     * @param callback a function that is invoked for each child in turn after
     *            it has been visited
     */
    public void visitComplexType(CobolComplexType type,
            ComplexTypeChildHandler callback) {

        cobolNamesStack.push(type.getCobolName());
        int index = 0;
        for (Entry < String, CobolType > child : type.getFields().entrySet()) {
            CobolType childType = child.getValue();
            String childName = child.getKey();
            curFieldName = childName;
            if (childType instanceof CobolOptionalType
                    && !isPresent((CobolOptionalType) childType)) {
                index++;
                continue;
            }
            if (!callback.preVisit(childName, index, childType)) {
                break;
            }

            childType.accept(this);

            if (!callback.postVisit(childName, index, childType)) {
                break;
            }
            index++;
        }
        cobolNamesStack.pop();
    }

    /**
     * Visit an array type which visits each items in turn.
     * <p/>
     * Size of the array might be inferred from an ODOObject visited before this
     * array.
     * 
     * @param type the array's item type
     * @param callback a function that is invoked for each item in turn after it
     *            has been visited
     */
    public void visitCobolArrayType(CobolArrayType type,
            ArrayTypeItemHandler callback) {
        for (int i = 0; i < getOccurs(type); i++) {
            if (!callback.preVisit(i, type)) {
                break;
            }
            cobolNamesStack.push("[" + i + "]");
            type.getItemType().accept(this);
            cobolNamesStack.pop();
            if (!callback.postVisit(i, type)) {
                break;
            }
        }
    }

    /**
     * Visit a choice type (COBOL REDEFINES) which determines which alternative
     * must be selected.
     * <p/>
     * 
     * @param choiceType the choice type
     * @param callback a function that is invoked after the selected alternative
     *            has been visited
     */
    public void visitCobolChoiceType(CobolChoiceType choiceType,
            ChoiceTypeAlternativeHandler callback) {

        CobolType alternative = null;

        // If we have a user provided strategy give it a chance to select a
        // alternative
        if (customChoiceStrategy != null) {
            alternative = customChoiceStrategy.choose(curFieldName, choiceType,
                    getVariables(), getHostData(), getLastPos());
        }

        // If user strategy did not succeed, try the default one
        if (alternative == null) {
            alternative = defaultChoiceStrategy.choose(curFieldName,
                    choiceType, getVariables(), getHostData(), getLastPos());
        }

        // All attempts have failed
        if (alternative == null) {
            throw new CobolChoiceStrategyException(
                    "Unable to select an alternative for choice "
                            + curFieldName + ", path: "
                            + getCurFieldFullCobolName());
        }

        // Make sure the strategy is returning a known alternative
        String alternativeName = choiceType.getAlternativeName(alternative);
        if (alternativeName == null) {
            throw new CobolChoiceStrategyException(
                    "Alternative does not correspond to a known alternative for choice "
                            + curFieldName + ", path: "
                            + getCurFieldFullCobolName());
        }

        callback.preVisit(alternativeName,
                choiceType.getAlternativeIndex(alternativeName), alternative);

        alternative.accept(this);

        // If the alternative chosen is not the largest one, add the difference
        // to an extra offset that will be used if a fixed position item follows
        // this choice
        if (alternative.getMaxBytesLen() < choiceType.getMaxBytesLen()) {
            extraOffset += choiceType.getMaxBytesLen()
                    - alternative.getMaxBytesLen();
        }

        callback.postVisit(alternativeName,
                choiceType.getAlternativeIndex(alternativeName), alternative);

    }

    /**
     * Visit a primitive type performing the actual conversion to a java object.
     * <p/>
     * If there is an extra offset left over by a previous item, adjust the
     * position accordingly.
     * 
     * @param type the primitive type
     * @param callback a function that is invoked after the primitive type has
     *            been visited
     * @throws FromCobolException
     */
    public void visitCobolPrimitiveType(CobolPrimitiveType < ? > type,
            PrimitiveTypeHandler callback) throws FromCobolException {

        if (extraOffset > 0) {
            this.lastPos += extraOffset;
            extraOffset = 0;
        }

        cobolNamesStack.push(type.getCobolName());
        callback.preVisit(type);

        FromHostPrimitiveResult < ? > result = type.fromHost(cobolContext,
                getHostData(), getLastPos());
        if (result.isSuccess()) {
            this.lastPos += type.getBytesLen();
        } else {
            throw new FromCobolException(result.getErrorMessage(),
                    getCurFieldFullCobolName());
        }

        // Keep those values that might be needed later
        if (type.isOdoObject() || isCustomVariable(type, curFieldName)) {
            putVariable(curFieldName, result.getValue());
        }

        callback.postVisit(type, result.getValue());
        cobolNamesStack.pop();

    }

    // -----------------------------------------------------------------------------
    // Notification interfaces
    // -----------------------------------------------------------------------------
    /**
     * Notifies specialized implementations when a complex type child has been
     * visited.
     */
    public interface ComplexTypeChildHandler {

        /**
         * Notify caller that a field is about to be visited
         * 
         * @param fieldName the visited field's name
         * @param fieldIndex the visited field's name position in its parent
         *            complex type
         * @param child the visited field type
         * @return if false, visiting should stop
         */
        boolean preVisit(String fieldName, int fieldIndex, CobolType child);

        /**
         * Notify caller that a field was visited
         * 
         * @param fieldName the visited field's name
         * @param fieldIndex the visited field's name position in its parent
         *            complex type
         * @param child the visited field type
         * @return if false, visiting should stop
         */
        boolean postVisit(String fieldName, int fieldIndex, CobolType child);

    }

    /**
     * Notifies specialized implementations when an array's item has been
     * visited.
     */
    public interface ArrayTypeItemHandler {

        /**
         * Notify caller that an item is about to be visited
         * 
         * @param itemIndex the visited item index in the array
         * @param item the visited item type
         * @return if false, visiting should stop
         */
        boolean preVisit(int itemIndex, CobolType item);

        /**
         * Notify caller that an item was visited
         * 
         * @param itemIndex the visited item index in the array
         * @param item the visited item type
         * @return if false, visiting should stop
         */
        boolean postVisit(int itemIndex, CobolType item);
    }

    /**
     * Notifies specialized implementations when an alternative has been
     * selected and visited
     */
    public interface ChoiceTypeAlternativeHandler {

        /**
         * Notify caller that an alternative is about to be visited
         * 
         * @param alternativeName the visited alternative field name in the
         *            choice
         * @param alternativeIndex the visited alternative position in the
         *            choice
         * @param alternative the visited alternative type
         */
        void preVisit(String alternativeName, int alternativeIndex,
                CobolType alternative);

        /**
         * Notify caller that an alternative was visited
         * 
         * @param alternativeName the visited alternative field name in the
         *            choice
         * @param alternativeIndex the visited alternative position in the
         *            choice
         * @param alternative the visited alternative type
         */
        void postVisit(String alternativeName, int alternativeIndex,
                CobolType alternative);
    }

    /**
     * Notifies specialized implementations when a primitive type has been
     * visited (and hence its value is known).
     */
    public interface PrimitiveTypeHandler {

        /**
         * Notify caller that a primitive type is about to be visited
         * 
         * @param type the primitive type
         */
        void preVisit(CobolPrimitiveType < ? > type);

        /**
         * Notify caller that a primitive type was visited
         * 
         * @param type the primitive type
         * @param value the primitive type value converted from mainframe data
         */
        void postVisit(CobolPrimitiveType < ? > type, Object value);
    }

    // -----------------------------------------------------------------------------
    // Internal methods
    // -----------------------------------------------------------------------------

    /**
     * Retrieve the size of an array.
     * <p/>
     * For variable size arrays this requires an ODOObject whose value
     * determines the size at runtime.
     * 
     * @param type the array type
     * @return the actual size of the array
     */
    private int getOccurs(CobolArrayType type) {
        if (type.isVariableSize()) {
            return getOdoValue(type.getDependingOn());
        } else {
            return type.getMaxOccurs();
        }
    }

    /**
     * Check if an optional type is present.
     * 
     * @param optionalType the optional type
     * @return true if the optional type is present
     */
    private boolean isPresent(CobolOptionalType optionalType) {
        if (optionalType.getDependingOn() != null) {
            return getOdoValue(optionalType.getDependingOn()) > 0;
        }
        return true;
    }

    /**
     * Retrieve the ODO object value for a variable size array.
     * <p/>
     * The ODO object has usually been populated before this method in invoked
     * in which case, its value has been stored as a variable.
     * <p/>
     * If we can't find the ODO object in the variables hash, this means it was
     * not populated. This is possible in case the ODO object is in a REDEFINE
     * alternative where that alternative was not chosen.
     * 
     * @param dependingOn the ODO object name
     * @return the value of the ODO object or zero if not found
     */
    private int getOdoValue(String dependingOn) {
        Object odoValue = variables.get(dependingOn);
        if (odoValue == null) {
            return 0;
        } else if (odoValue instanceof Number) {
            return ((Number) odoValue).intValue();
        } else {
            throw new CobolODOResolutionException("The value "
                    + odoValue.toString() + " for ODOObject " + dependingOn
                    + " si not numeric" + ". Path: "
                    + getCurFieldFullCobolName());
        }
    }

    // -----------------------------------------------------------------------------
    // Getters/Setters
    // -----------------------------------------------------------------------------
    /**
     * Incoming mainframe data.
     * 
     * @return Incoming mainframe data
     */
    public byte[] getHostData() {
        return hostData;
    }

    /**
     * Where to start processing in the incoming mainframe data buffer.
     * 
     * @return Where to start processing in the incoming mainframe data buffer
     */
    public int getStart() {
        return start;
    }

    /**
     * After the visitor is done visiting, this will contain the last position
     * in the incoming mainframe data buffer that was processed.
     * 
     * @return last position in the incoming mainframe data buffer that was
     *         processed
     */
    public int getLastPos() {
        return lastPos;
    }

    /**
     * Specialized implementations might need to advance the last position.
     * 
     * @param lastPos last position in the incoming mainframe byte array
     */
    public void setLastPos(int lastPos) {
        this.lastPos = lastPos;
    }

    /**
     * Keep track of a variable's value
     * 
     * @param name the variable name
     * @param value the variable value
     */
    public void putVariable(String name, Object value) {
        this.variables.put(name, value);
    }

    /**
     * The set of custom variables and ODOObjects collected while visiting.
     * 
     * @return a map of variable names to their values
     */
    public Map < String, Object > getVariables() {
        return variables;
    }

    /**
     * A primitive type is a custom variable if it has been marked as so via one
     * of the available mechanisms or is needed to make choice decisions.
     * 
     * @param type the primitive type
     * @param name the variable name
     * @return true if this is a custom variable
     */
    public boolean isCustomVariable(CobolPrimitiveType < ? > type, String name) {
        if (type.isCustomVariable()) {
            return true;
        }
        if (customVariables != null && customVariables.contains(name)) {
            return true;
        }
        if (customChoiceStrategy != null
                && customChoiceStrategy.getVariableNames() != null
                && customChoiceStrategy.getVariableNames().contains(name)) {
            return true;
        }
        return false;
    }

    /**
     * Field being visited. The name identifies this field to its immediate
     * parent.
     * 
     * @return name of field being visited
     */
    public String getCurFieldName() {
        return curFieldName;
    }

    /**
     * @return the name of the current field with ancestor path
     */
    public String getCurFieldFullCobolName() {
        StringBuffer sb = new StringBuffer();
        Iterator < String > it = cobolNamesStack.iterator();
        while (it.hasNext()) {
            String cobolName = it.next();
            if (StringUtils.isBlank(cobolName)) {
                continue;
            }
            if (sb.length() > 0 && !cobolName.startsWith("[")) {
                sb.append("/");
            }
            sb.append(cobolName);
        }
        return sb.toString();
    }

    public CobolContext getCobolContext() {
        return cobolContext;
    }

}
