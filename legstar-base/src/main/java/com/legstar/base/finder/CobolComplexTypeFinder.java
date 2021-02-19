package com.legstar.base.finder;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.utils.NumUtils;
import com.legstar.base.visitor.Cob2ObjectValidator;
import com.legstar.base.visitor.MaxBytesLenCobolVisitor;

/**
 * This finder implementation assumes a complex type starts with a number of
 * fields that have recognizable formats and boundaries. All together these
 * fields form the Type's signature.
 * <p>
 * This finder is constructed using a Cobol complex type and a stop field which
 * delimits the number of fields at the start of the complex type forming the
 * type's 'signature'.
 * <p>
 * Note that some Cobol types are easier to recognize than others, for instance
 * {@link com.legstar.base.type.primitive.CobolPackedDecimalType} and
 * {@link com.legstar.base.type.primitive.CobolZonedDecimalType} impose severe
 * limitations on their content and therefore are unlikely to be missed for some
 * other type. So you want to have as many as possible of these in your
 * signature.
 * <p>
 * Decimal types can be defined with boundaries (minInclusive and maxInclusive)
 * which are very useful to avoid the risk of a mismatch.
 * <p>
 * Note that this implementation will not work if the signature part of the type
 * contains variable size arrays (DEPENDING ON) or choices (REDEFINES).
 *
 */
public class CobolComplexTypeFinder extends CobolTypeFinder {

    /**
     * Host COBOL configuration parameters.
     */
    private final CobolContext cobolContext;

    /** Structure that starts with the signature. */
    private final CobolComplexType cobolComplexType;

    /** Where does the signature stop. */
    private final String stopFieldInclusive;

    /** How many bytes are needed to uniquely identify a type. */
    private final int signatureLen;

    /** The complex type maximum size in host bytes */
    private final long maxBytesLen;

    /** The complex type minimum size in host bytes */
    private final long minBytesLen;

    /**
     * Construct the finder.
     * 
     * @param cobolContext host COBOL configuration parameters
     * @param cobolComplexType the Cobol complex type we are looking for
     */
    public CobolComplexTypeFinder(CobolContext cobolContext,
            CobolComplexType cobolComplexType) {
        this(cobolContext, cobolComplexType, null);
    }

    /**
     * Construct the finder.
     * 
     * @param cobolContext host COBOL configuration parameters
     * @param cobolComplexType the Cobol complex type we are looking for
     * @param stopFieldInclusive what is the last field of the signature (the
     *            signature is formed by all fields from the start of the
     *            complex type up to this one). If you pass null or a field that
     *            is not in the complex type, the entire complex type form the
     *            signature.
     */
    public CobolComplexTypeFinder(CobolContext cobolContext,
            CobolComplexType cobolComplexType, String stopFieldInclusive) {

        this.cobolContext = cobolContext;
        this.cobolComplexType = cobolComplexType;
        this.stopFieldInclusive = stopFieldInclusive;
        minBytesLen = cobolComplexType.getMinBytesLen();
        maxBytesLen = cobolComplexType.getMaxBytesLen();

        MaxBytesLenCobolVisitor maxBytesLenVisitor = new MaxBytesLenCobolVisitor(
                stopFieldInclusive);
        maxBytesLenVisitor.visit(cobolComplexType);
        signatureLen = NumUtils.longToInt(maxBytesLenVisitor.getMaxBytesLen());

    }

    /** {@inheritDoc} */
    public boolean match(byte[] hostData, int start, int length) {

        Cob2ObjectValidator visitor = new Cob2ObjectValidator(cobolContext,
                hostData, start, length, stopFieldInclusive);
        visitor.visit(cobolComplexType);
        return visitor.isValid();

    }

    /** {@inheritDoc} */
    public int getSignatureLen() {
        return signatureLen;
    }

    public CobolComplexType getCobolComplexType() {
        return cobolComplexType;
    }

    public String getStopFieldInclusive() {
        return stopFieldInclusive;
    }

    public long getMinBytesLen() {
        return minBytesLen;
    }

    public long getMaxBytesLen() {
        return maxBytesLen;
    }

}
