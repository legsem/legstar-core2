package com.legstar.converter.type.primitive;

import java.nio.ByteBuffer;

import com.legstar.converter.context.CobolContext;
import com.legstar.converter.type.FromHostException;

/**
 * Binary (COMP)
 * 
 */
public class CobolBinaryType<T extends Number> extends CobolDecimalType < T > {

    public static final int MAX_TOTAL_DIGITS = 18;

    
   /**
     * Size of the buffer needed to hold 2's complement java values of this
     * type.
     */
    private final int bufferLen;

    /** {@inheritDoc} */
    protected boolean isValidInternal(Class < T > clazz, byte[] hostData,
            int start) {
        // Unsigned numeric must have sign bit turned off
        if (!isSigned() && isNegative(hostData[start])) {
            return false;
        }

        return true;
    }

    /** {@inheritDoc} */
    protected T fromHostInternal(Class < T > clazz, byte[] hostData, int start) {
        ByteBuffer bb = getByteBuffer(clazz, hostData, start);
        return valueOf(clazz, bb, getFractionDigits());
    }

    /**
     * Copies the mainframe data into a byte buffer large enough for the target
     * java Number and sets position to zero so that value can be extracted
     * straight away.
     * 
     * 
     * @param clazz the java Number type
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return a byte buffer with mainframe data.
     */
    private ByteBuffer getByteBuffer(Class < T > clazz, byte[] hostData,
            int start) {

        int hostBytesLen = getBytesLen();
        int pos = start;

        ByteBuffer bb = ByteBuffer.allocate(bufferLen);

        if (bb.capacity() > hostBytesLen) {

            // More java capacity than host bytes
            // Left pad depending on sign
            byte padByte = (isSigned() && isNegative(hostData[start])) ? (byte) 0xFF
                    : (byte) 0x00;
            for (int i = 0; i < bb.capacity() - hostBytesLen; i++) {
                bb.put(padByte);
            }

        } else {

            if (bb.capacity() < hostBytesLen) {
                // More host bytes than java capacity
                // If host first bytes are insignificant, we might still be
                // able to fit in the desired java Type
                int leading00 = 0;
                int leadingFF = 0;
                int excessBytes = hostBytesLen - bb.capacity();
                while (pos < start + excessBytes) {
                    if (hostData[pos] == (byte) 0x00) {
                        leading00++;
                    } else if (hostData[pos] == (byte) 0xFF) {
                        leadingFF++;
                    }
                    pos++;
                }

                if ((leading00 != excessBytes)
                        && (leadingFF != excessBytes || !isNegative(hostData[pos]))) {
                    throw new FromHostException(
                            "Host "
                                    + hostBytesLen
                                    + " bytes numeric is too large for the target java type "
                                    + clazz.getName(), hostData, start);
                }
            }

            // If the MSB is set for an unsigned numeric then it is not going to
            // fit in the target java Number (which is always signed)
            if (!isSigned() && isNegative(hostData[pos])) {
                throw new FromHostException("Host unsigned " + hostBytesLen
                        + " bytes numeric"
                        + " is too large for the target java type "
                        + clazz.getName(), hostData, start);
            }

        }

        bb.put(hostData, pos, hostBytesLen - (pos - start));
        bb.position(0);
        return bb;
    }

    /** {@inheritDoc} */
    public int getBytesLen() {
        return getBytesLen(getTotalDigits());
    }

    public static int getBytesLen(int totalDigits) {
        return totalDigits <= 4 ? 2 : (totalDigits <= 9 ? 4 : 8);
    }

    private boolean isNegative(byte b) {
        return (b >> 7 & 1) == 1;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T extends Number> extends
            CobolDecimalType.Builder < T, Builder < T >> {

        public Builder(CobolContext cobolContext, Class < T > clazz) {
            super(cobolContext, clazz, MAX_TOTAL_DIGITS);
        }

        public CobolBinaryType < T > build() {
            return new CobolBinaryType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolBinaryType(Builder < T > builder) {
        super(builder);
        
        // Determine the byte buffer length needed to hold the 2's complement
        // value for this java type
        if (getClazz().equals(Short.class)) {
            bufferLen = 2;
        } else if (getClazz().equals(Integer.class)) {
            bufferLen = 4;
        } else if (getClazz().equals(Long.class)) {
            bufferLen = 8;
        } else {
            bufferLen = 8 + (isSigned() ? 0 : 1);
        }
    }

}
