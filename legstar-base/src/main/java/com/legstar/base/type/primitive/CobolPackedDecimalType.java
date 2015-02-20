package com.legstar.base.type.primitive;

import com.legstar.base.context.CobolContext;

/**
 * A Packed Decimal (COMP-3).
 * 
 */
public class CobolPackedDecimalType<T extends Number> extends
        CobolDecimalType < T > {

    // COMP-3 might have up to 31 digits (with arith(extend))
    public static final int MAX_TOTAL_DIGITS = 31;

    /** {@inheritDoc} */
    protected boolean isValidInternal(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int end = start + getBytesLen();

        // examine nibbles in each byte in turn
        int[] nibbles = new int[2];

        // all nibbles, except the last one, must hold valid digits
        for (int i = start; i < end - 1; i++) {
            setNibbles(nibbles, hostData[i]);
            if (!isDigit(nibbles[0]) || !isDigit(nibbles[1])) {
                return false;
            }
        }

        // Check last byte
        setNibbles(nibbles, hostData[end - 1]);
        if (!isDigit(nibbles[0])) {
            return false;
        }
        // Last byte must hold a valid sign (in the right nibble)
        if (isSigned()) {
            if (nibbles[1] != cobolContext.getPositiveSignNibbleValue()
                    && nibbles[1] != cobolContext.getNegativeSignNibbleValue()) {
                return false;
            }
        } else {
            if (nibbles[1] != cobolContext.getUnspecifiedSignNibbleValue()) {
                return false;
            }
        }

        return true;
    }

    /** {@inheritDoc} */
    protected FromHostPrimitiveResult < T > fromHostInternal(
            Class < T > javaClass, CobolContext cobolContext, byte[] hostData,
            int start) {

        int end = start + getBytesLen();

        StringBuffer sb = new StringBuffer();
        int[] nibbles = new int[2];

        for (int i = start; i < end; i++) {
            setNibbles(nibbles, hostData[i]);
            char digit0 = getDigit(nibbles[0]);
            if (digit0 == '\0') {
                return new FromHostPrimitiveResult < T >(
                        "First nibble is not a digit", hostData, start, i,
                        getBytesLen());
            }
            sb.append(digit0);
            if (i == end - 1) {
                if (isSigned()) {
                    if (nibbles[1] == cobolContext.getNegativeSignNibbleValue()) {
                        sb.insert(0, "-");
                    } else if (nibbles[1] != cobolContext
                            .getPositiveSignNibbleValue()) {
                        return new FromHostPrimitiveResult < T >(
                                "Nibble at sign position does not contain the expected values 0x"
                                        + Integer.toHexString(cobolContext
                                                .getNegativeSignNibbleValue())
                                        + " or 0x"
                                        + Integer.toHexString(cobolContext
                                                .getPositiveSignNibbleValue()),
                                hostData, start, i, getBytesLen());
                    }
                } else if (nibbles[1] != cobolContext
                        .getUnspecifiedSignNibbleValue()) {
                    return new FromHostPrimitiveResult < T >(
                            "Nibble at sign position does not contain the expected value 0x"
                                    + Integer.toHexString(cobolContext
                                            .getUnspecifiedSignNibbleValue()),
                            hostData, start, i, getBytesLen());
                }
            } else {
                char digit1 = getDigit(nibbles[1]);
                if (digit1 == '\0') {
                    return new FromHostPrimitiveResult < T >(
                            "Second nibble is not a digit", hostData, start, i,
                            getBytesLen());
                }
                sb.append(digit1);
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
        return getBytesLen(getTotalDigits());
    }

    public static int getBytesLen(int totalDigits) {
        return (totalDigits + 2) / 2;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T extends Number> extends
            CobolDecimalType.Builder < T, Builder < T >> {

        public Builder(Class < T > clazz) {
            super(clazz, MAX_TOTAL_DIGITS);
        }

        public CobolPackedDecimalType < T > build() {
            return new CobolPackedDecimalType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolPackedDecimalType(Builder < T > builder) {
        super(builder);
    }

}
