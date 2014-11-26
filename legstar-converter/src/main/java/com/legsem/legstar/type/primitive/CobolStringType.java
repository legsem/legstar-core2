package com.legsem.legstar.type.primitive;

import java.io.UnsupportedEncodingException;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.FromHostException;
import com.legsem.legstar.type.FromHostResult;

/**
 * A PIC X known to contain readable characters.
 * 
 */
public class CobolStringType extends CobolPrimitiveType < String > {

    private final int charNum;

    /** {@inheritDoc} */
    public boolean isValid(byte[] hostData, int start) {

        int bytesLen = getBytesLen();

        // Is buffer large enough to contain this type?
        // TODO last field in a record might be truncated if all low-values or
        // spaces
        if (hostData.length < start + bytesLen) {
            return false;
        }

        return isValidString(hostData, start);
    }

    /**
     * Relatively naive implementation that assumes content is either low-value
     * or a code point greater or equal to the space character.
     * 
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid string
     */
    protected boolean isValidString(byte[] hostData, int start) {

        int length = start + getBytesLen();

        for (int i = start; i < length; i++) {
            int code = hostData[i] & 0xFF;
            if (code != 0 && code < getHostSpaceCharCode()) {
                return false;
            }
        }

        return true;
    }

    /** {@inheritDoc} */
    public FromHostResult < String > fromHost(byte[] hostData, int start)
            throws FromHostException {

        int bytesLen = getBytesLen();

        // For strings it is acceptable that host is sending over less data
        // than expected. This happens when trailing low values are chopped
        // off by transports
        if (hostData.length < start + bytesLen) {
            bytesLen = hostData.length - start;
        }

        // Trim trailing low-values and, optionally, trailing spaces
        int spaceCode = getHostSpaceCharCode();
        boolean checkSpace = isTruncateHostStringsTrailingSpaces();

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
                    work[i - start] = hostData[i] == 0 ? (byte) getHostSpaceCharCode()
                            : hostData[i];
                }
                result = new String(work, getHostCharsetName());
            } else {
                result = new String(hostData, start, end - start,
                        getHostCharsetName());
            }
        } catch (UnsupportedEncodingException e) {
            throw new FromHostException("Failed to use host character set "
                    + getHostCharsetName(), hostData, start, e);
        }

        return new FromHostResult < String >(bytesLen, result);
    }

    public int getBytesLen() {
        return charNum;
    }

    public int getHostSpaceCharCode() {
        return getCobolContext().getHostSpaceCharCode();
    }

    public String getHostCharsetName() {
        return getCobolContext().getHostCharsetName();
    }

    public int getMaxPicXCharnum() {
        return getCobolContext().getMaxPicXCharnum();
    }

    public boolean isTruncateHostStringsTrailingSpaces() {
        return getCobolContext().isTruncateHostStringsTrailingSpaces();
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder extends CobolPrimitiveType.Builder<String, Builder> {

        private int charNum;

        public Builder(CobolContext cobolContext) {
            super(cobolContext);
        }

        public Builder charNum(int value) {
            charNum = value;
            return this;
        }

       public CobolStringType build() {
            return new CobolStringType(this);
        }

       protected Builder self() {
           return this;
       }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolStringType(Builder builder) {
        
        super(builder);

        if (builder.charNum < 0) {
            throw new IllegalArgumentException("Characters number " + builder.charNum
                    + " cannot be negative");
        }

        if (builder.charNum > getMaxPicXCharnum()) {
            throw new IllegalArgumentException("Characters number " + builder.charNum
                    + " is greater than maximum " + getMaxPicXCharnum());
        }

        charNum = builder.charNum;
    }

}