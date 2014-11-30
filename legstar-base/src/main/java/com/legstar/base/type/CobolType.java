package com.legstar.base.type;

import com.legstar.base.visitor.CobolVisitor;

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
