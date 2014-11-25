package com.legsem.legstar.visitor;

import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.ConversionException;
import com.legsem.legstar.type.FromHostResult;
import com.legsem.legstar.type.composite.CobolArrayType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolPrimitiveType;

/**
 * Validates that an incoming mainframe bytes array contains data that is
 * compatible with a given complex type.
 * <p/>
 * Validation can be stopped early by specifying a stop field name. All fields
 * up to this one (inclusive) are validated. The rest of the data is ignored.
 * <p/>
 * Data corresponding to individual fields is validated without necessarily
 * performing the conversion. However, if the field is defined with a range
 * (minInclusive, maxInclusive) then data is converted to check that it fits in
 * the required range.
 * <p/>
 * Upon completion, the lastPos property indicates the last position in the
 * incoming buffer that was validated.
 * 
 */
public class ValidateFromCobolVisitor extends FromCobolVisitor {

    /**
     * Overwritten by each visited item.
     */
    private boolean valid;

    /**
     * Optionally indicates on which field to stop validation
     */
    private final String stopFieldInclusive;

    private ComplexTypeChildHandler complexTypeChildHandler = new ComplexTypeChildHandler() {

        public boolean postVisit(String fieldName, CobolType child) {
            return valid && !fieldName.equals(stopFieldInclusive);
        }

    };

    private ArrayTypeItemHandler arrayTypeItemHandler = new ArrayTypeItemHandler() {

        public boolean postVisit(CobolType item) {
            return valid;
        }

    };

    private ChoiceTypeAlternativeHandler choiceTypeAlternativeHandler = new ChoiceTypeAlternativeHandler() {

        public void postVisit(String alternativeName, CobolType alternative) {

        }

    };

    public ValidateFromCobolVisitor(byte[] hostData, int start) {
        this(hostData, start, null, null);
    }

    public ValidateFromCobolVisitor(byte[] hostData, int start,
            String stopFieldInclusive) {
        this(hostData, start, stopFieldInclusive, null);
    }

    public ValidateFromCobolVisitor(byte[] hostData, int start,
            String stopFieldInclusive,
            FromCobolChoiceStrategy customChoiceStrategy) {
        super(hostData, start, customChoiceStrategy);
        this.stopFieldInclusive = stopFieldInclusive;
        this.valid = true;
    }

    public void visit(CobolComplexType type) throws ConversionException {

        super.visitComplexType(type, complexTypeChildHandler);

    }

    public void visit(CobolArrayType type) throws ConversionException {

        super.visitCobolArrayType(type, arrayTypeItemHandler);
    }

    public void visit(CobolChoiceType type) throws ConversionException {

        super.visitCobolChoiceType(type, choiceTypeAlternativeHandler);

    }

    /**
     * The logic is similar to
     * {@link FromCobolVisitor#visitCobolPrimitiveType(CobolPrimitiveType, PrimitiveTypeHandler)}
     * but we try to avoid converting data unless this is needed for range
     * comparison or because the field is a custom variable or an ODOObject.
     * 
     * <p/>
     * {@inheritDoc}
     */
    public void visit(CobolPrimitiveType < ? > type) throws ConversionException {
        if (type.isValid(getHostData(), getLastPos())) {

            // Perform conversion for those values that might be needed later
            if (type.isOdoObject() || type.isCustomVariable()) {
                FromHostResult < ? > result = type.fromHost(getHostData(),
                        getLastPos());
                putVariable(getCurFieldName(), result.getValue());
                setLastPos(getLastPos() + result.getBytesProcessed());
            } else {
                setLastPos(getLastPos() + type.getBytesLen());
            }

        } else {
            valid = false;
        }
    }

    public boolean isValid() {
        return valid;
    }

}
