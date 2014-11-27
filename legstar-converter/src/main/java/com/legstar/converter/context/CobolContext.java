package com.legstar.converter.context;

public interface CobolContext {

    /**
     * @return code for unspecified sign nibble value
     */
    public int getUnspecifiedSignNibbleValue();

    /**
     * @return code for positive sign nibble value
     */
    public int getPositiveSignNibbleValue();

    /**
     * @return code for négative sign nibble value
     */
    public int getNegativeSignNibbleValue();

    /**
     * @return code for a separate plus sign
     */
    public int getHostPlusSign();

    /**
     * @return code for a separate minus sign
     */
    public int getHostMinusSign();

    /**
     * @return the host space character code point
     */
    public int getHostSpaceCharCode();

    /**
     * @return the character set used by the host to encode strings (Must be a
     *         valid java charset)
     */
    public String getHostCharsetName();

    /**
     * @return should trailing spaces be trimmed from host strings when
     *         converted to java
     */
    public boolean isTruncateHostStringsTrailingSpaces();

}
