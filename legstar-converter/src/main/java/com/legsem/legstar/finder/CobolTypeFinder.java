package com.legsem.legstar.finder;

/**
 * A finder looks up the signature of a Cobol type in a byte stream.
 * <p/>
 * This is useful when you are looking for a particular Cobol type (typically a
 * complex type) in a byte stream that otherwise contains data you are not
 * interested in.
 *
 */
public abstract class CobolTypeFinder {

    /**
     * Lookup for the signature of a Cobol type.
     * <p/>
     * Attempts at finding the type one byte at a time delegating the matching
     * to specialized classes.
     * 
     * @param hostData the incoming host data where the matching type might
     *            appear
     * @param start where to start looking in the incoming host data
     * @param length where to stop looking in the incoming host data
     * @return the position of the match or -1 if there is no match
     */
    public int indexOf(byte[] hostData, int start, int length) {
        int pos = start;
        while (length - pos >= signatureLen()) {
            if (match(hostData, pos, length)) {
                return pos;
            }
            pos++;
        }
        return -1;
    }

    /**
     * Match a Cobol type to an incoming host bytes set.
     * 
     * @param hostData the host bytes set
     * @param start where to start matching
     * @param length the size of the incoming host bytes set. This should be large
     *            enough to hold a signature but is not guaranteed to hold more
     *            than that.
     * @return true if the Cobol type is compatible with the host bytes at the
     *         given start position
     */
    public abstract boolean match(byte[] hostData, int start, int length);

    /**
     * @return the byte length of the signature (total number of bytes needed to
     *         match the type)
     */
    public abstract int signatureLen();
}
