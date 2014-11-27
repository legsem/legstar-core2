package com.legstar.converter.type;

import com.legstar.converter.visitor.CobolVisitor;

/**
 * Represents a COBOL item type.
 * <p/>
 * They represent metadata and does not hold the actual item value during
 * conversion.
 * <p/>
 * Types are immutable and thread safe.
 * 
 */
public abstract class CobolType {

    /**
     * Propagate a visitor.
     * 
     * @param visitor the visitor
     */
    public abstract void accept(CobolVisitor visitor);


}
