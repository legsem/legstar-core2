package com.legstar.base.context;

/**
 * ASCII COBOL context.
 *
 */
public class AsciiCobolContext extends AbstractCobolContext {

    public static final int UNSPECIFIED_SIGN_VALUE = 0x0f;
    public static final int POSITIVE_SIGN_VALUE = 0x0c;
    public static final int NEGATIVE_SIGN_VALUE = 0x0d;
    private static final int ASCII_PLUS_SIGN = 0x2b;
    private static final int ASCII_MINUS_SIGN = 0x2d;
    private static final int ASCII_SPACE_CODE = 0x20;

    public AsciiCobolContext() {
        this("ISO-8859-1");
    }

    public AsciiCobolContext(String HostCharsetName) {
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
        return ASCII_PLUS_SIGN;
    }

    public int getHostMinusSign() {
        return ASCII_MINUS_SIGN;
    }

    public int getHostSpaceCharCode() {
        return ASCII_SPACE_CODE;
    }

    public boolean isTruncateHostStringsTrailingSpaces() {
        return true;
    }
}
