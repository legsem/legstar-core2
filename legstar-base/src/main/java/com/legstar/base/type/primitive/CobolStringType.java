package com.legstar.base.type.primitive;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

import com.legstar.base.FromHostException;
import com.legstar.base.FromHostResult;
import com.legstar.base.context.CobolContext;

/**
 * A PIC X.
 * 
 */
public class CobolStringType<T> extends CobolPrimitiveType < T > {

    /**
     * @return maximum number of characters in a PIC X.
     */
    private static final int MAXIMUM_PICX_CHARNUM = 16777215;

    private final int charNum;

    /** {@inheritDoc} */
    public boolean isValid(Class < T > javaClass, CobolContext cobolContext,
            byte[] hostData, int start) {

        int bytesLen = getBytesLen(charNum);

        // Is buffer large enough to contain this type?
        // TODO last field in a record might be truncated if all low-values or
        // spaces
        if (hostData.length < start + bytesLen) {
            return false;
        }

        if (javaClass.equals(String.class)) {
            return isValidString(cobolContext, hostData, start);
        } else if (javaClass.equals(ByteBuffer.class)) {
            return true;
        } else {
            throw new IllegalArgumentException("Unsupported java type "
                    + javaClass);
        }
    }

    /**
     * Relatively naive implementation that assumes content is either low-value
     * or a code point greater or equal to the space character.
     * 
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid string
     */
    protected boolean isValidString(CobolContext cobolContext, byte[] hostData,
            int start) {

        int length = start + getBytesLen();

        for (int i = start; i < length; i++) {
            int code = hostData[i] & 0xFF;
            if (code != 0 && code < cobolContext.getHostSpaceCharCode()) {
                return false;
            }
        }

        return true;
    }

    /** {@inheritDoc} */
    public FromHostResult < T > fromHost(Class < T > javaClass, CobolContext cobolContext,
            byte[] hostData, int start) throws FromHostException {

        int bytesLen = getBytesLen();

        // For strings it is acceptable that host is sending over less data
        // than expected. This happens when trailing low values are chopped
        // off by transports
        if (hostData.length < start + bytesLen) {
            bytesLen = hostData.length - start;
        }
        
        return fromHostInternal(javaClass, cobolContext, hostData, start, bytesLen);

    }

    @SuppressWarnings("unchecked")
    private <D> FromHostResult < D > fromHostInternal(Class < D > javaClass,
            CobolContext cobolContext, byte[] hostData, int start, int bytesLen) {
        if (javaClass.equals(String.class)) {
            return (FromHostResult < D >) fromHostString(cobolContext,
                    hostData, start, bytesLen);
        } else if (javaClass.equals(ByteBuffer.class)) {
            return (FromHostResult < D >) fromHostBytes(cobolContext,
                    hostData, start, bytesLen);
        } else {
            throw new IllegalArgumentException("Unsupported java type " + javaClass);
        }

    }

    /**
     * Convert host data to a java String.
     * 
     * @param cobolContext the COBOL config parameters
     * @param hostData the host data
     * @param start where to start in the host data
     * @param bytesLen how many bytes from the host data to process
     * @return the result of the conversion
     * @throws FromHostException if conversion fails
     */
    private FromHostResult < String > fromHostString(CobolContext cobolContext,
            byte[] hostData, int start, int bytesLen) throws FromHostException {

        // Trim trailing low-values and, optionally, trailing spaces
        int spaceCode = cobolContext.getHostSpaceCharCode();
        boolean checkSpace = cobolContext.isTruncateHostStringsTrailingSpaces();

        int end = start + bytesLen;
        while (end > start) {
            int code = hostData[end - 1] & 0xFF;
            if (code != 0 && ((checkSpace && code != spaceCode) || !checkSpace)) {
                break;
            }
            end--;
        }

        // Return early if this is an empty string
        if (start == end) {
            return new FromHostResult < String >(bytesLen, "");
        }

        // Host strings are sometimes dirty
        boolean isDirty = false;
        for (int i = start; i < end; i++) {
            if (hostData[i] == 0) {
                isDirty = true;
                break;
            }
        }

        // Cleanup if needed then ask java to convert using the proper host
        // character set
        String result = null;
        try {
            if (isDirty) {
                byte[] work = new byte[end - start];
                for (int i = start; i < end; i++) {
                    work[i - start] = hostData[i] == 0 ? (byte) cobolContext
                            .getHostSpaceCharCode() : hostData[i];
                }
                result = new String(work, cobolContext.getHostCharsetName());
            } else {
                result = new String(hostData, start, end - start,
                        cobolContext.getHostCharsetName());
            }
        } catch (UnsupportedEncodingException e) {
            throw new FromHostException("Failed to use host character set "
                    + cobolContext.getHostCharsetName(), hostData, start, e);
        }

        return new FromHostResult < String >(bytesLen, result);
    }

    /**
     * Case where the host data must be preserved as is (no conversion)
     * 
     * @param cobolContext the COBOL context
     * @param hostData the host data
     * @param start where to start in the host data
     * @param bytesLen how many bytes from the host data to process
     * @return the result of the conversion
     * @throws FromHostException if conversion fails
     */
    private FromHostResult < ByteBuffer > fromHostBytes(
            CobolContext cobolContext, byte[] hostData, int start, int bytesLen)
            throws FromHostException {

        ByteBuffer result = ByteBuffer.allocate(bytesLen);
        result.put(hostData, start, bytesLen);
        result.rewind();
        return new FromHostResult < ByteBuffer >(bytesLen, result);
    }

    public int getBytesLen() {
        return getBytesLen(charNum);
    }

    public static int getBytesLen(int charNum) {
        return charNum;
    }
    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<T> extends
            CobolPrimitiveType.Builder < T, Builder < T > > {

        private int charNum;

        public Builder(Class < T > clazz) {
            super(clazz);
        }

        public Builder < T > charNum(int value) {
            charNum = value;
            return this;
        }

        public CobolStringType < T > build() {
            return new CobolStringType < T >(this);
        }

        protected Builder < T > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolStringType(Builder < T > builder) {

        super(builder);

        if (builder.charNum < 0) {
            throw new IllegalArgumentException("Characters number "
                    + builder.charNum + " cannot be negative");
        }

        if (builder.charNum > MAXIMUM_PICX_CHARNUM) {
            throw new IllegalArgumentException("Characters number "
                    + builder.charNum + " is greater than maximum "
                    + MAXIMUM_PICX_CHARNUM);
        }

        charNum = builder.charNum;
    }

}
