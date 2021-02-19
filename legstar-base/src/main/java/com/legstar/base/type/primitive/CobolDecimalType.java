package com.legstar.base.type.primitive;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Arrays;

import com.legstar.base.context.CobolContext;

/**
 * A generic type for mainframe decimals.
 * <p>
 * This is a broad class that covers pretty much all COBOL numerics apart from
 * Float/Double.
 * <p>
 * The parameterized type <T> determines which java Number type this COBOL
 * decimal is converted to.
 * 
 */
public abstract class CobolDecimalType<T extends Number> extends
        CobolPrimitiveType < T > {

    protected static final char JAVA_DECIMAL_POINT = '.';

    /**
     * True if this is a signed COBOL decimal.
     */
    private final boolean signed;

    /**
     * Total number of digits (including the fraction digits)
     */
    private final int totalDigits;

    /**
     * Number of fraction digits (0 for an integer)
     */
    private final int fractionDigits;

    /**
     * Optional lower limit for the value.
     */
    private final T minInclusive;

    /**
     * Optional upper limit for the value.
     */
    private final T maxInclusive;

    /** Java characters corresponding to digits. */
    private static final char[] JAVA_DIGITS = new char[] { '0', '1', '2', '3',
            '4', '5', '6', '7', '8', '9' };

    /** {@inheritDoc} */
    public final boolean isValid(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int hostBytesLen = getBytesLen();

        // Is buffer large enough to contain this type?
        if (hostData.length < start + hostBytesLen) {
            return false;
        }

        // Let specialized classes perform their validation
        if (!isValidInternal(javaClass, cobolContext, hostData, start)) {
            return false;
        }

        // If required to check within range, need to convert
        FromHostPrimitiveResult < T > result = fromHostInternal(javaClass,
                cobolContext, hostData, start);
        return result.isSuccess() && isWithinRange(result.getValue());

    }

    /**
     * Check value against required range (if any).
     * 
     * @param value the value that must fall within the range
     * @return false if value is outside the range
     */
    private boolean isWithinRange(T value) {
        if (getMinInclusive() != null
                && compareTo(getMinInclusive(), value) > 0) {
            return false;
        }
        if (getMaxInclusive() != null
                && compareTo(getMaxInclusive(), value) < 0) {
            return false;
        }
        return true;
    }

    /**
     * Check if a byte array contains a valid type with this instance
     * characteristics.
     * 
     * @param javaClass java Number class mapped to this COBOL decimal
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid type
     */
    protected abstract boolean isValidInternal(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start);

    /** {@inheritDoc} */
    public FromHostPrimitiveResult < T > fromHost(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int hostBytesLen = getBytesLen();
        if (hostData.length < start + hostBytesLen) {
            return new FromHostPrimitiveResult < T >("Length provided "
                    + (hostData.length - start)
                    + " is smaller than the required " + hostBytesLen, hostData,
                    start, hostBytesLen);
        }

        FromHostPrimitiveResult < T > result = fromHostInternal(javaClass,
                cobolContext, hostData, start);
        if (result.isSuccess()) {
            if (!isWithinRange(result.getValue())) {
                return new FromHostPrimitiveResult < T >("Value "
                        + result.getValue().toString()
                        + " is outside the required range "
                        + getRangeAsString(), hostData, start, hostBytesLen);
            }
        }

        return result;
    }

    /**
     * Convert mainframe data into a java Number.
     * 
     * @param javaClass java Number class mapped to this COBOL decimal
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the conversion results, including the mainframe value as a java
     *         object if conversion succeeded
     */
    protected abstract FromHostPrimitiveResult < T > fromHostInternal(
            Class < T > javaClass, CobolContext cobolContext, byte[] hostData,
            int start);

    /**
     * A nibble is a half byte.
     * 
     * @param nibbles to be populated
     * @param hostByte the incoming byte
     */
    public void setNibbles(int[] nibbles, byte hostByte) {
        nibbles[0] = (hostByte & 0xF0) >>> 4;
        nibbles[1] = hostByte & 0x0F;
    }

    /**
     * Convert a nibble content to a java digit.
     * 
     * @param index the nibble content is a value between 0 and 9 givig the
     *            index of the digit
     * @return the java digit corresponding to the nibble value or null if the
     *         nibble is not a digit
     */
    public char getDigit(int index) {
        return index < 0 || index >= JAVA_DIGITS.length ? '\0'
                : JAVA_DIGITS[index];
    }

    /**
     * Check if a nibble contains a valid digit.
     * 
     * @param nibble the nibble to check
     * @return true if this nibble is a valid digit
     */
    public boolean isDigit(int nibble) {
        return nibble > -1 && nibble < 10;
    }

    /**
     * Given a string representation of a numeric value, convert that to the
     * target java Number type.
     * <p>
     * If the target is an integer type, we trim any fractional part.
     * 
     * @param clazz the java Number type
     * @param str the string representation of a numeric value
     * @return the java Number obtained from the input string
     * @throws NumberFormatException if umber cannot be derived from the input
     *             string
     */
    @SuppressWarnings("unchecked")
    public static <D extends Number> D valueOf(Class < D > clazz, String str) {
        if (clazz.equals(Short.class)) {
            return (D) Short.valueOf(intPart(str));
        } else if (clazz.equals(Integer.class)) {
            return (D) Integer.valueOf(intPart(str));
        } else if (clazz.equals(Long.class)) {
            return (D) Long.valueOf(intPart(str));
        } else if (clazz.equals(BigDecimal.class)) {
            return (D) new BigDecimal(str);
        } else if (clazz.equals(BigInteger.class)) {
            return (D) new BigInteger(intPart(str));
        }
        throw new IllegalArgumentException("Unsupported java type " + clazz);
    }

    /**
     * Given a byte buffer containing the 2's complement representation of an
     * unscaled numeric, convert that to the target java Number type.
     * <p>
     * Note that there is an assumption that the byte buffer contains at least 8
     * 
     * @param clazz the java Number type
     * @param bb byte buffer containing the 2's complement representation of an
     *            unscaled numeric
     * @param fractionDigits the number of fraction digits in the COBOL type
     * @return java Number type corresponding to byte content
     */
    @SuppressWarnings("unchecked")
    public static <D extends Number> D valueOf(Class < D > clazz,
            ByteBuffer bb, int fractionDigits) {
        if (clazz.equals(Short.class)) {
            return (D) Short.valueOf(bb.getShort());
        } else if (clazz.equals(Integer.class)) {
            return (D) Integer.valueOf(bb.getInt());
        } else if (clazz.equals(Long.class)) {
            return (D) Long.valueOf(bb.getLong());
        } else if (clazz.equals(BigDecimal.class)) {
            if (bb.capacity() <= 8) {
                return (D) BigDecimal.valueOf(bb.getLong(), fractionDigits);
            } else {
                return (D) new BigDecimal(new BigInteger(bb.array()),
                        fractionDigits);
            }
        }
        if (clazz.equals(BigInteger.class)) {
            return (D) new BigInteger(bb.array());
        } else {
            throw new IllegalArgumentException("Unsupported java type " + clazz);
        }

    }

    /**
     * Removes the fractional part from a numeric's string representation.
     * 
     * @param str the numeric string decimal representation
     * @return the decimal representation without the factional part
     */
    private static String intPart(String str) {
        return str.indexOf(JAVA_DECIMAL_POINT) > 0 ? str.substring(0,
                str.indexOf(JAVA_DECIMAL_POINT)) : str;
    }

    /**
     * Determines the maximum value a numeric can take.
     * <p>
     * The maximum is constrained either from the host side by the the number of
     * digits in the picture clause or by the mapping java type maximum value
     * whichever is smaller.
     * 
     * @param clazz the target java numeric type
     * @param totalDigits the host total number of digits
     * @param fractionDigits the host fractional number of digits
     * @return the maximum value this java numeric can take
     */
    @SuppressWarnings("unchecked")
    private static <D extends Number> D getJavaMaxInclusive(Class < D > clazz,
            int totalDigits, int fractionDigits) {
        int dec = totalDigits - fractionDigits;
        char[] decPart = new char[dec];
        Arrays.fill(decPart, '9');
        StringBuilder sb = new StringBuilder();
        sb.append(decPart);
        if (clazz.equals(Short.class)) {
            return (D) (dec > 4 ? Short.valueOf(Short.MAX_VALUE) : Short
                    .valueOf(sb.toString()));
        } else if (clazz.equals(Integer.class)) {
            return (D) (dec > 9 ? Integer.valueOf(Integer.MAX_VALUE) : Integer
                    .valueOf(sb.toString()));
        } else if (clazz.equals(Long.class)) {
            return (D) (dec > 18 ? Long.valueOf(Long.MAX_VALUE) : Long
                    .valueOf(sb.toString()));
        } else if (clazz.equals(BigInteger.class)) {
            return (D) new BigInteger(sb.toString());
        } else if (clazz.equals(BigDecimal.class)) {
            if (fractionDigits > 0) {
                sb.append(JAVA_DECIMAL_POINT);
                char[] fracPart = new char[fractionDigits];
                Arrays.fill(fracPart, '9');
                sb.append(fracPart);
            }
            return (D) new BigDecimal(sb.toString());

        } else {
            throw new IllegalArgumentException("Unsupported java type " + clazz);
        }

    }

    /**
     * Determines the minimum value a numeric can take.
     * <p>
     * The minimum is constrained either from the host side by the the number of
     * digits in the picture clause or by the mapping java type minimum value
     * whichever is larger.
     * 
     * @param clazz the target java numeric type
     * @param signed whether this numeric is signed
     * @param totalDigits the host total number of digits
     * @param fractionDigits the host fractional number of digits
     * @return the minimum value this java numeric can take
     */
    @SuppressWarnings("unchecked")
    private static <D extends Number> D getJavaMinInclusive(Class < D > clazz,
            boolean signed, int totalDigits, int fractionDigits) {

        if (!signed) {
            return valueOf(clazz, "0");
        }

        int dec = totalDigits - fractionDigits;
        char[] decPart = new char[dec];
        Arrays.fill(decPart, '9');
        StringBuilder sb = new StringBuilder();
        sb.append('-');
        sb.append(decPart);
        if (clazz.equals(Short.class)) {
            return (D) (dec > 4 ? Short.valueOf(Short.MIN_VALUE) : Short
                    .valueOf(sb.toString()));
        } else if (clazz.equals(Integer.class)) {
            return (D) (dec > 9 ? Integer.valueOf(Integer.MIN_VALUE) : Integer
                    .valueOf(sb.toString()));
        } else if (clazz.equals(Long.class)) {
            return (D) (dec > 18 ? Long.valueOf(Long.MIN_VALUE) : Long
                    .valueOf(sb.toString()));
        } else if (clazz.equals(BigInteger.class)) {
            return (D) new BigInteger(sb.toString());
        } else if (clazz.equals(BigDecimal.class)) {
            if (fractionDigits > 0) {
                sb.append(JAVA_DECIMAL_POINT);
                char[] fracPart = new char[fractionDigits];
                Arrays.fill(fracPart, '9');
                sb.append(fracPart);
            }
            return (D) new BigDecimal(sb.toString());

        } else {
            throw new IllegalArgumentException("Unsupported java type " + clazz);
        }

    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static <D extends Number> int compareTo(D obj1, D obj2) {
        return ((Comparable) obj1).compareTo((Comparable) obj2);
    }

    public boolean isSigned() {
        return signed;
    }

    public int getTotalDigits() {
        return totalDigits;
    }

    public int getFractionDigits() {
        return fractionDigits;
    }

    public T getMinInclusive() {
        return minInclusive;
    }

    public T getMaxInclusive() {
        return maxInclusive;
    }

    /**
     * @return a log friendly string showing the range this numeric must fall in
     */
    private String getRangeAsString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append(getMinInclusive() == null ? "none" : getMinInclusive()
                .toString());
        sb.append(", ");
        sb.append(getMaxInclusive() == null ? "none" : getMaxInclusive()
                .toString());
        sb.append("]");
        return sb.toString();
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public abstract static class Builder<T extends Number, B extends Builder < T, B >>
            extends CobolPrimitiveType.Builder < T, B > {

        // Required
        protected final int maxTotalDigits;

        // Optional
        protected boolean signed = false;
        protected int totalDigits = 9;
        protected int fractionDigits = 0;
        protected T minInclusive = null;
        protected T maxInclusive = null;

        protected boolean minInclusiveSetFlag = false;
        protected boolean maxInclusiveSetFlag = false;

        public Builder(Class < T > clazz, int maxTotalDigits) {
            super(clazz);
            this.maxTotalDigits = maxTotalDigits;
        }

        public B signed(boolean value) {
            signed = value;
            return self();
        }

        public B totalDigits(int value) {
            totalDigits = value;
            return self();
        }

        public B fractionDigits(int value) {
            fractionDigits = value;
            return self();
        }

        public B minInclusive(T value) {
            minInclusive = value;
            minInclusiveSetFlag = true;
            return self();
        }

        public B maxInclusive(T value) {
            maxInclusive = value;
            maxInclusiveSetFlag = true;
            return self();
        }

        protected abstract B self();

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    public CobolDecimalType(Builder < T, ? > builder) {
        super(builder);

        if (builder.totalDigits < 0) {
            throw new IllegalArgumentException("Total digits number "
                    + builder.totalDigits + " cannot be negative");
        }
        if (builder.totalDigits == 0) {
            throw new IllegalArgumentException("Total digits cannot be zero");
        }
        if (builder.totalDigits > builder.maxTotalDigits) {
            throw new IllegalArgumentException("Total digits number "
                    + builder.totalDigits + " cannot exceed "
                    + builder.maxTotalDigits);
        }
        if (builder.fractionDigits < 0) {
            throw new IllegalArgumentException("Fraction digits number "
                    + builder.fractionDigits + " cannot be negative");
        }
        if (builder.totalDigits < builder.fractionDigits) {
            throw new IllegalArgumentException("Fraction digits number "
                    + builder.fractionDigits
                    + " cannot exceed total digits number "
                    + builder.totalDigits);
        }
        signed = builder.signed;
        totalDigits = builder.totalDigits;
        fractionDigits = builder.fractionDigits;
        minInclusive = builder.minInclusiveSetFlag ? builder.minInclusive
                : getJavaMinInclusive(builder.javaClass, builder.signed,
                        builder.totalDigits, builder.fractionDigits);
        maxInclusive = builder.maxInclusiveSetFlag ? builder.maxInclusive
                : getJavaMaxInclusive(builder.javaClass, builder.totalDigits,
                        builder.fractionDigits);
    }

}
