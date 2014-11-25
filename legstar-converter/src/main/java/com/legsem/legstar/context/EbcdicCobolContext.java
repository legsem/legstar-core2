package com.legsem.legstar.context;

/**
 * Z/OS EBCDIC COBOL context.
 *
 */
public class EbcdicCobolContext extends AbstractCobolContext {

    public static final int UNSPECIFIED_SIGN_VALUE = 0x0f;
    public static final int POSITIVE_SIGN_VALUE = 0x0c;
    public static final int NEGATIVE_SIGN_VALUE = 0x0d;
    private static final int EBCDIC_PLUS_SIGN = 0x4e;
    private static final int EBCDIC_MINUS_SIGN = 0x60;
    private static final int EBCDIC_SPACE_CODE = 0x40;
    private static final int MAXIMUM_PICX_CHARNUM = 16777215;

    
    public EbcdicCobolContext() {
        this("IBM01140");
    }

    public EbcdicCobolContext(String HostCharsetName) {
        super(HostCharsetName);
    }

    public int getUnspecifiedSignNibbleValue() {
        return UNSPECIFIED_SIGN_VALUE;
    }

    public int getPositiveSignNibbleValue() {
        return POSITIVE_SIGN_VALUE;
    }

    public int getNegativeSignNibbleValue() {
        return NEGATIVE_SIGN_VALUE;
    }

    public int getHostPlusSign() {
        return EBCDIC_PLUS_SIGN;
    }

    public int getHostMinusSign() {
        return EBCDIC_MINUS_SIGN;
    }

    public int getHostSpaceCharCode() {
        return EBCDIC_SPACE_CODE;
    }

    public int getMaxPicXCharnum() {
        return MAXIMUM_PICX_CHARNUM;
    }

    public boolean isTruncateHostStringsTrailingSpaces() {
        return true;
    }

}
