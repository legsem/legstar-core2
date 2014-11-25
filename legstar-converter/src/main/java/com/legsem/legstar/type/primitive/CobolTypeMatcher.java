package com.legsem.legstar.type.primitive;

/**
 * Determines if a given byte array contains a certain COBOL type.
 * 
 */
public interface CobolTypeMatcher {

    /**
     * @param hostData the bytes to test for a match
     * @param start where to start matching
     * @param length total length of the buffer
     * @return true if the start position matches the start of a given COBOL
     *         type.
     */
    boolean doMatch(byte[] hostData, int start, int length);

}
