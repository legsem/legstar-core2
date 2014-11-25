package com.legsem.legstar.type.primitive;

/**
 * Matcher for Packed Decimals (COMP-3).
 *
 */
public class CobolPackedDecimalMatcher implements CobolTypeMatcher {
    
    public boolean doMatch(byte[] hostData, int start, int length) {

        // COMP-3 might have up to 31 digits (with arith(extend))
        if (length == 0 || length > 32) {
            return false;
        }

        // Last byte must hold a valid sign
        int s = (hostData[start + length - 1] & 0x0F);
        if (s != 0x0d && s != 0x0c && s != 0x0f) {
            return false;
        }

        // all other half bytes must hold valid digits
        int[] d = new int[2];
        for (int i = start; i < length; i++) {

            d[0] = (hostData[i] & 0xF0) >>> 4;
            d[1] = hostData[i] & 0x0F;

            if (d[0] < 0 || d[0] > 9) {
                return false;
            }
            if (i < length - 1 && (d[1] < 0 || d[1] > 9)) {
                return false;
            }
        }
        return true;

        
    }

}
