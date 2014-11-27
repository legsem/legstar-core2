package com.legstar.converter.type;

/**
 * Aggregate result of converting host bytes to java.
 * 
 */
public class FromHostResult<J> {

    /** how many host bytes were used to produce the java object */
    private final int bytesProcessed;

    /**the java object produced by the conversion */
    private final J value;
    
    public FromHostResult(int bytesProcessed, J value) {
        this.bytesProcessed = bytesProcessed;
        this.value = value;
    }

    public int getBytesProcessed() {
        return bytesProcessed;
    }

    public J getValue() {
        return value;
    }


}
