package com.legstar.base.visitor;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.ConversionException;
import com.legstar.base.type.FromHostException;
import com.legstar.base.type.FromHostResult;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

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

    // -----------------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------------
    public FromCobolVisitor(CobolContext cobolContext, byte[] hostData,
            int start) {
        this(cobolContext, hostData, start, null);
    }

    public FromCobolVisitor(CobolContext cobolContext, byte[] hostData,
            int start, FromCobolChoiceStrategy customChoiceStrategy) {
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
        variables = new HashMap < String, Object >();
        this.customChoiceStrategy = customChoiceStrategy;
        this.defaultChoiceStrategy = new DefaultFromCobolChoiceStrategy(
                cobolContext);
    }

    // -----------------------------------------------------------------------------
    // Visiting logic available to specific implementations
    // -----------------------------------------------------------------------------
    /**
     * Visit a complex type which visits each of its children in turn.
     * 
     * @param type the complex type to visit
     * @param callback a function that is invoked for each child in turn after
     *            it has been visited
     */
    public void visitComplexType(CobolComplexType type,
            ComplexTypeChildHandler callback) {
        for (Entry < String, CobolType > child : type.getFields().entrySet()) {
            curFieldName = child.getKey();
            child.getValue().accept(this);
            if (!callback.postVisit(child.getKey(), child.getValue())) {
                break;
            }
        }
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
            type.getItemType().accept(this);
            if (!callback.postVisit(type)) {
                break;
            }
        }
    }

    /**
     * Visit a choice type (COBOL REDEFINES) which determines which alternative
     * must be selected.
     * <p/>
     * 
     * @param type the choice type
     * @param callback a function that is invoked after the selected alternative
     *            has been visited
     * @throws ConversionException
     */
    public void visitCobolChoiceType(CobolChoiceType type,
            ChoiceTypeAlternativeHandler callback) throws ConversionException {

        CobolType alternative = null;

        // If we have a user provided strategy give it a chance to select a
        // alternative
        if (customChoiceStrategy != null) {
            alternative = customChoiceStrategy.choose(curFieldName, type,
                    getVariables(), getHostData(), getLastPos());
        }

        // If user strategy did not succeed, try the default one
        if (alternative == null) {
            alternative = defaultChoiceStrategy.choose(curFieldName, type,
                    getVariables(), getHostData(), getLastPos());
        }

        // All attempts have failed
        if (alternative == null) {
            throw new FromHostException(
                    "Unable to select an alternative for choice "
                            + curFieldName, getHostData(), getLastPos());
        }

        // Make sure the strategy is returning a known alternative
        String alternativeName = type.getNamesMap().get(alternative);
        if (alternativeName == null) {
            throw new FromHostException(
                    "Alternative does not correspond to a known alternative for choice "
                            + curFieldName, getHostData(), getLastPos());
        }

        alternative.accept(this);

        callback.postVisit(alternativeName, alternative);

    }

    /**
     * Visit a primitive type performing the actual conversion to a java object.
     * 
     * @param type the primitive type
     * @param callback a function that is invoked after the primitive type has
     *            been visited
     * @throws ConversionException
     */
    public void visitCobolPrimitiveType(CobolPrimitiveType < ? > type,
            PrimitiveTypeHandler callback) throws ConversionException {

        FromHostResult < ? > result = type.fromHost(cobolContext,
                getHostData(), getLastPos());
        this.lastPos += result.getBytesProcessed();

        // Keep those values that might be needed later
        if (type.isOdoObject() || type.isCustomVariable()) {
            putVariable(curFieldName, result.getValue());
        }

        callback.postVisit(type, result.getValue());

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
         * Notify caller that a field was visited
         * 
         * @param fieldName the visited field's name
         * @param child the visited field type
         * @return if false, visiting should stop
         */
        boolean postVisit(String fieldName, CobolType child);

    }

    /**
     * Notifies specialized implementations when an array's item has been
     * visited.
     */
    public interface ArrayTypeItemHandler {

        /**
         * Notify caller that an item was visited
         * 
         * @param item the visited item type
         * @return if false, visiting should stop
         */
        boolean postVisit(CobolType item);
    }

    /**
     * Notifies specialized implementations when an alternative has been
     * selected and visited
     */
    public interface ChoiceTypeAlternativeHandler {

        /**
         * Notify caller that an alternative was visited
         * 
         * @param alternativeName the visited alternative field name in the
         *            choice
         * @param alternative the visited alternative type
         */
        void postVisit(String alternativeName, CobolType alternative);
    }

    /**
     * Notifies specialized implementations when a primitive type has been
     * visited (and hence its value is known).
     */
    public interface PrimitiveTypeHandler {

        /**
         * Notify caller that a primitive type was visited
         * 
         * @param type the primitive type
         * @param value the primitive type value converted from mainframe data
         */
        void postVisit(CobolType type, Object value);
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
            Object odoValue = variables.get(type.getDependingOn());
            if (odoValue == null) {
                throw new FromHostException("No value available for ODOObject "
                        + type.getDependingOn()
                        + ". Variable size array cannot be sized",
                        getHostData(), getLastPos());
            } else if (odoValue instanceof Number) {
                return ((Number) odoValue).intValue();
            } else {
                throw new FromHostException("The value " + odoValue.toString()
                        + " for ODOObject " + type.getDependingOn()
                        + " si not numeric.", getHostData(), getLastPos());
            }
        } else {
            return type.getMaxOccurs();
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
     * @param lastPos last position in the inciming mainframe byte array
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
     * Field being visited. The name identifies this field to its immediate
     * parent.
     * 
     * @return name of field being visited
     */
    public String getCurFieldName() {
        return curFieldName;
    }

    /**
     * @param curFieldName name of field being visited
     */
    public void setCurFieldName(String curFieldName) {
        this.curFieldName = curFieldName;
    }

    public CobolContext getCobolContext() {
        return cobolContext;
    }

}
