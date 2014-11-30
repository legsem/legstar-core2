package com.legstar.base.type;

import com.legstar.base.utils.HexUtils;

/**
 * Exception converting data from host to java.
 * 
 */
public class FromHostException extends ConversionException {

    private static final int SPYBUF_MAX_LEN = 16;
    private static final long serialVersionUID = -7405334512989284423L;

    public FromHostException(String message, byte[] hostData,
            int curPos, Throwable cause) {
        super(formatMessage(message, hostData, curPos), cause);
    }

    public FromHostException(String message, byte[] hostData,
            int curPos) {
        super(formatMessage(message, hostData, curPos));
    }

    private static String formatMessage(String message, byte[] hostData,
            int curPos) {

        StringBuilder sb = new StringBuilder();
        sb.append(message);
        sb.append(". Position is " + curPos);
        if (hostData != null && hostData.length > 0 && curPos < hostData.length) {
            sb.append(". Data at position 0x");
            int spyStart = Math.max(curPos - SPYBUF_MAX_LEN, 0);
            appendData(sb, hostData, spyStart, curPos - spyStart);
            sb.append("->");
            appendData(sb, hostData, curPos, 1);
            int spyStop = Math.min(curPos + SPYBUF_MAX_LEN, hostData.length);
            appendData(sb, hostData, curPos + 1, spyStop - curPos - 1);
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
