package com.legstar.base.type.primitive;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.FromHostException;
import com.legstar.base.type.FromHostResult;

/**
 * COBOL Double type.
 * <p/>
 * This is the bit layout for this type:
 * 
 * <pre>
6666555555555544444444443333333333222222222211111111110000000000
3210987654321098765432109876543210987654321098765432109876543210
----------------------------------------------------------------
0100010101010100001110101110100100010101101101010111001111101000
-                                                                sign
 -------                                                         excess
        -------------------------------------------------------  mantissa
 * </pre>
 *
 */
public class CobolDoubleType<T extends Number> extends CobolPrimitiveType < T > {

    public FromHostResult < T > fromHost(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start)
            throws FromHostException {

        int bytesLen = getBytesLen();
        if (hostData.length < start + bytesLen) {
            throw new FromHostException("Length provided "
                    + (hostData.length - start)
                    + " is smaller than the required " + bytesLen, hostData,
                    start);
        }

        ByteBuffer bb = ByteBuffer.wrap(hostData, start, bytesLen);
        long hostLongBits = bb.getLong();

        /* First bit left (bit 63) is the sign: 0 = positive, 1 = negative */
        int sign = (int) ((hostLongBits & 0x8000000000000000L) >>> 63);

        /*
         * Bits 62-56 (7 bits) represents the exponent offset by 64, this
         * number is called excess so you get the exponent as
         * E= excess - 64
         */
        int excess = (int) ((hostLongBits & 0x7f00000000000000L) >>> 56);
        int exponent = excess == 0 ? 0 : (excess - 64);

        /* Bits 55-0 (56 bits) represents the mantissa. */
        long mantissa = hostLongBits & 0x00ffffffffffffffL;

        /* Java Doubles are in IEEE 754 floating-point bit layout. */
        /*
         * Host exponent is hexadecimal based while java is binary based.
         * There is also an additional shift for non-zero values due to
         * the 1-plus" normalized java specs.
         */
        if (mantissa != 0) {
            exponent = ((4 * exponent) - 1);
        }

        /*
         * The java mantissa is 53 bits while the host is 56. This
         * means there is a systematic loss of precision.
         */
        mantissa = mantissa >>> 3;
        
        /* In java the 53th bit needs to be one */
        while (mantissa > 0L && (mantissa & 0x0010000000000000L) == 0) {
            mantissa = mantissa << 1;
            exponent = exponent - 1;
        }

        /* First check if this is a zero value */
        double result = 0d;
        if (exponent != 0 || mantissa != 0) {
            /* Get rid of the leading 1 which needs to be implicit */
            long javaLongBits = mantissa & 0x000fffffffffffffL;
            javaLongBits = javaLongBits | ((long) (exponent + 1023) << 52);
            javaLongBits = javaLongBits | ((long) sign << 63);
            result = Double.longBitsToDouble(javaLongBits);
        }

        return new FromHostResult < T >(bytesLen, valueOf(javaClass, result));
    }

    @SuppressWarnings("unchecked")
    public static <D extends Number> D valueOf(Class < D > clazz, Double value) {
        if (clazz.equals(Double.class)) {
            return (D) value;
        } else if (clazz.equals(Float.class)) {
            return (D) Float.valueOf(value.floatValue());
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
        return 8;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T extends Number> extends
            CobolPrimitiveType.Builder < T, Builder < T > > {

        public Builder(Class < T > clazz) {
            super(clazz);
        }

        public CobolDoubleType < T > build() {
            return new CobolDoubleType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolDoubleType(Builder < T > builder) {

        super(builder);

    }

}
