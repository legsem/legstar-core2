package com.legstar.base.type.primitive;

import com.legstar.base.context.CobolContext;

/**
 * A Zoned Decimal (PIC S9(n)v9(m) DISPLAY).
 * 
 */
public class CobolZonedDecimalType<T extends Number> extends
        CobolDecimalType < T > {

    // Zoned decimals might have up to 31 digits (with arith(extend))
    public static final int MAX_TOTAL_DIGITS = 31;

    private final boolean signLeading;

    private final boolean signSeparate;

    /** {@inheritDoc} */
    public boolean isValidInternal(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int end = start + getBytesLen();

        int[] nibbles = new int[2];

        // check all bytes excluding sign
        // all right hand size nibbles must contain a digit
        for (int i = start + (signLeading ? 1 : 0); i < end
                - (signLeading ? 0 : 1); i++) {
            setNibbles(nibbles, hostData[i]);
            if (!isDigit(nibbles[1])) {
                return false;
            }
        }

        // check the sign
        if (signSeparate) {
            int separateSign = (signLeading ? hostData[start]
                    : hostData[end - 1]) & 0xFF;
            if (separateSign != cobolContext.getHostPlusSign()
                    && separateSign != cobolContext.getHostMinusSign()) {
                return false;
            }
        } else {
            setNibbles(nibbles, signLeading ? hostData[start]
                    : hostData[end - 1]);
            if (!isDigit(nibbles[1])) {
                return false;
            }
            if (isSigned()) {
                if (nibbles[0] != cobolContext.getPositiveSignNibbleValue()
                        && nibbles[0] != cobolContext
                                .getNegativeSignNibbleValue()) {
                    return false;
                }
            } else {
                if (nibbles[0] != cobolContext.getUnspecifiedSignNibbleValue()) {
                    return false;
                }
            }
        }

        return true;
    }

    /** {@inheritDoc} */
    protected FromHostPrimitiveResult < T > fromHostInternal(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int end = start + getBytesLen();

        StringBuffer sb = new StringBuffer();
        int[] nibbles = new int[2];

        for (int i = start + (signLeading ? 1 : 0); i < end
                - (signLeading ? 0 : 1); i++) {
            setNibbles(nibbles, hostData[i]);
            char digit1 = getDigit(nibbles[1]);
            if (digit1 == '\0') {
                return new FromHostPrimitiveResult < T >(
                        "Second nibble is not a digit", hostData, start, i, getBytesLen());
            }
            sb.append(digit1);
        }

        int signPos = signLeading ? start : end - 1;
        if (signSeparate) {
            int separateSign = hostData[signPos] & 0xFF;
            if (separateSign == cobolContext.getHostMinusSign()) {
                sb.insert(0, "-");
            } else if (separateSign != cobolContext.getHostPlusSign()) {
                return new FromHostPrimitiveResult < T >("Found character "
                        + Integer.toHexString(separateSign)
                        + " where a sign was expected",
                        hostData, start, signPos, getBytesLen());
            }
        } else {
            setNibbles(nibbles, hostData[signPos]);
            char digit1 = getDigit(nibbles[1]);
            if (digit1 == '\0') {
                return new FromHostPrimitiveResult < T >(
                        "Second nibble is not a digit", hostData, start, signPos, getBytesLen());
            }
            sb.append(digit1);
            if (isSigned()
                    && nibbles[0] == cobolContext.getNegativeSignNibbleValue()) {
                sb.insert(0, "-");
            }
        }

        if (getFractionDigits() > 0) {
            sb.insert(sb.length() - getFractionDigits(), JAVA_DECIMAL_POINT);
        }

        try {
            T value = valueOf(javaClass, sb.toString());
            return new FromHostPrimitiveResult < T >(value);
        } catch (NumberFormatException e) {
            return new FromHostPrimitiveResult < T >("Host " + getMaxBytesLen()
                    + " bytes numeric converts to '" + sb.toString()
                    + "' which is not a valid " + javaClass.getName(),
                    hostData, start, getBytesLen());
        }
    }

    /** {@inheritDoc} */
    public int getBytesLen() {
        return getBytesLen(getTotalDigits(), isSignSeparate());
    }

    public static int getBytesLen(int totalDigits, boolean signSeparate) {
        return totalDigits + (signSeparate ? 1 : 0);
    }

    public boolean isSignLeading() {
        return signLeading;
    }

    public boolean isSignSeparate() {
        return signSeparate;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T extends Number> extends
            CobolDecimalType.Builder < T, Builder < T >> {

        private boolean signLeading;
        private boolean signSeparate;

        public Builder(Class < T > clazz) {
            super(clazz, MAX_TOTAL_DIGITS);
        }

        public Builder < T > signLeading(boolean value) {
            signLeading = value;
            return this;
        }

        public Builder < T > signSeparate(boolean value) {
            signSeparate = value;
            return this;
        }

        public CobolZonedDecimalType < T > build() {
            return new CobolZonedDecimalType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolZonedDecimalType(Builder < T > builder) {

        super(builder);

        if (builder.signSeparate && !isSigned()) {
            throw new IllegalArgumentException(
                    "Sign cannot be separate for unsigned decimal");
        }

        if (builder.signLeading && !isSigned()) {
            throw new IllegalArgumentException(
                    "Sign cannot be leading for unsigned decimal");
        }

        signLeading = builder.signLeading;
        signSeparate = builder.signSeparate;
    }

}
