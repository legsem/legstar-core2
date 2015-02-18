package com.legstar.base.type.primitive;

import com.legstar.base.utils.HexUtils;

/**
 * Result of converting host bytes to java for a primitive type.
 * 
 */
public class FromHostPrimitiveResult<J> {

    private static final int SPYBUF_MAX_LEN = 16;

    /** The java object produced by the conversion */
    private final J value;

    /** Was the conversion successful */
    private final boolean success;

    /** When conversion fails, describes the error */
    private final String errorMessage;

    public boolean isSuccess() {
        return success;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public FromHostPrimitiveResult(J value) {
        this.value = value;
        this.success = true;
        this.errorMessage = null;
    }

    public FromHostPrimitiveResult(String message, byte[] hostData, int start,
            int bytesLen) {
        this(message, hostData, start, start, bytesLen);
    }

    public FromHostPrimitiveResult(String message, byte[] hostData, int start,
            int curPos, int bytesLen) {
        this.value = null;
        this.success = false;
        this.errorMessage = formatMessage(message, hostData, start, curPos,
                bytesLen);
    }

    public J getValue() {
        return value;
    }

    /**
     * Format the data to make it easy to locate the error in the incoming byte
     * stream.
     * 
     * @param message the text part of the message (explains the error)
     * @param hostData the incoming host data stream
     * @param start the starting position of the primitive type value in the
     *            byte stream
     * @param curPos the position of the error itself (might be within the type
     *            value)
     * @param bytesLen the size of the primitive type
     * @return a formatted message
     */
    protected static String formatMessage(String message, byte[] hostData,
            int start, int curPos, int bytesLen) {

        StringBuilder sb = new StringBuilder();
        sb.append(message);
        if (hostData != null && hostData.length > 0 && curPos < hostData.length
                && curPos >= start && bytesLen > 0
                && start + bytesLen <= hostData.length) {
            sb.append(". Error at offset ");
            sb.append(curPos);
            sb.append(" : [0x");
            int spyStart = Math.max(start - SPYBUF_MAX_LEN, 0);
            appendData(sb, hostData, spyStart, start - spyStart);
            sb.append(start > spyStart ? "->" : "");
            if (curPos > start) {
                appendData(sb, hostData, start, curPos - start);
                sb.append("^");
                appendData(sb, hostData, curPos, bytesLen + start - curPos);
            } else {
                appendData(sb, hostData, start, bytesLen);
            }
            int spyStop = Math.min(start + bytesLen + SPYBUF_MAX_LEN,
                    hostData.length);
            sb.append(spyStop > start + bytesLen ? "<-" : "");
            appendData(sb, hostData, start + bytesLen, spyStop - start
                    - bytesLen);
            sb.append("]");
        } else {
            sb.append(". Position is ");
            sb.append(curPos);
        }
        return sb.toString();

    }

    private static void appendData(StringBuilder sb, byte[] hostData,
            int start, int spyLen) {
        if (spyLen <= 0 || spyLen > SPYBUF_MAX_LEN) {
            return;
        }
        byte[] spy = new byte[spyLen];
        System.arraycopy(hostData, start, spy, 0, spyLen);
        sb.append(HexUtils.encodeHex(spy));
    }

}
