package com.legstar.base.type.primitive;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;

import com.legstar.base.context.CobolContext;

/**
 * COBOL Float type.
 * <p/>
 * This is the bit layout for this type:
 * 
 * <pre>
 * 33222222222211111111110000000000
 * 10987654321098765432109876543210
 * --------------------------------
 * 01000011010011010010000000000000
 * -                                 sign
 *  -------                          excess
 *         ------------------------  mantissa
 * </pre>
 * 
 *
 */
public class CobolFloatType<T extends Number> extends CobolPrimitiveType < T > {

    public FromHostPrimitiveResult < T > fromHost(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start) {

        int hostBytesLen = getBytesLen();
        if (hostData.length < start + hostBytesLen) {
            return new FromHostPrimitiveResult < T >("Length provided "
                    + (hostData.length - start)
                    + " is smaller than the required " + hostBytesLen, hostData,
                    start, hostBytesLen);
        }

        ByteBuffer bb = ByteBuffer.wrap(hostData, start, hostBytesLen);
        int hostIntBits = bb.getInt();
        int sign = (hostIntBits & 0x80000000) >>> 31;

        /*
         * Bits 30-24 (7 bits) represents the exponent offset by 64, this number
         * is called excess so you get the exponent as E= excess - 64
         */
        int excess = (hostIntBits & 0x7f000000) >>> 24;
        int exponent = excess == 0 ? 0 : (excess - 64);

        /* Bits 23-0 (24 bits) represents the getMantissa(). */
        int mantissa = hostIntBits & 0x00ffffff;

        /* Java Floats are in IEEE 754 floating-point bit layout. */
        /*
         * Host exponent is hexadecimal based while java is binary based. There
         * is also an additional shift for non-zero values due to the 1-plus"
         * normalized java specs.
         */
        if (mantissa != 0) {
            exponent = ((4 * exponent) - 1);
        }

        /* In java the 24th bit needs to be one */
        while ((mantissa > 0) && (mantissa & 0x00800000) == 0) {
            mantissa = mantissa << 1;
            exponent = exponent - 1;
        }

        /* First check if this is a zero value */
        float result = 0f;
        if (exponent != 0 || mantissa != 0) {
            /* Get rid of the leading 1 which needs to be implicit */
            int javaIntBits = mantissa & 0x007fffff;
            javaIntBits = javaIntBits | ((exponent + 127) << 23);
            javaIntBits = javaIntBits | (sign << 31);
            result = Float.intBitsToFloat(javaIntBits);
        }

        return new FromHostPrimitiveResult < T >(valueOf(javaClass, result));
    }

    @SuppressWarnings("unchecked")
    public static <D extends Number> D valueOf(Class < D > clazz, Float value) {
        if (clazz.equals(Float.class)) {
            return (D) value;
        } else if (clazz.equals(Double.class)) {
            return (D) Double.valueOf((double) value);
        } else if (clazz.equals(Short.class)) {
            return (D) Short.valueOf(value.shortValue());
        } else if (clazz.equals(Integer.class)) {
            return (D) Integer.valueOf(value.intValue());
        } else if (clazz.equals(Long.class)) {
            return (D) Long.valueOf(value.longValue());
        } else if (clazz.equals(BigDecimal.class)) {
            return (D) new BigDecimal(value);
        } else if (clazz.equals(BigInteger.class)) {
            return (D) BigInteger.valueOf(value.longValue());
        }
        throw new IllegalArgumentException("Unsupported java type " + clazz);
    }

    public boolean isValid(Class < T > javaClass, CobolContext cobolContext,
            byte[] hostData, int start) {
        int bytesLen = getBytesLen();

        // Is buffer large enough to contain this type?
        // TODO last field in a record might be truncated if all low-values or
        // spaces
        if (hostData.length < start + bytesLen) {
            return false;
        }
        // FIXME requires specific validation
        return true;
    }

    public int getBytesLen() {
        return 4;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T extends Number> extends
            CobolPrimitiveType.Builder < T, Builder < T > > {

        public Builder(Class < T > clazz) {
            super(clazz);
        }

        public CobolFloatType < T > build() {
            return new CobolFloatType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolFloatType(Builder < T > builder) {

        super(builder);

    }

}
