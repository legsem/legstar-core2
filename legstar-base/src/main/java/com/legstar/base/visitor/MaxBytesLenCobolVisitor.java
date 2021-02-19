package com.legstar.base.visitor;

import java.util.Map.Entry;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

/**
 * Calculates the maximum number of bytes needed for a mainframe buffer to hold
 * a representation of a complex type.
 * <p>
 * Variable size arrays are considered to have all occurrences.
 * <p>
 * Choices are assumed to have the largest alternative selected.
 * <p>
 * Optionally calculation can stop on a field in the middle of the structure
 * (calculates partial length).
 * 
 */
public class MaxBytesLenCobolVisitor implements CobolVisitor {

    long maxBytesLen = 0;

    /**
     * Optionally indicates on which field to stop calculation
     */
    private final String stopFieldInclusive;

    public MaxBytesLenCobolVisitor() {
        this(null);
    }

    public MaxBytesLenCobolVisitor(String stopFieldInclusive) {
        this.stopFieldInclusive = stopFieldInclusive;
    }

    public void visit(CobolComplexType type) {
        for (Entry < String, CobolType > child : type.getFields().entrySet()) {
            child.getValue().accept(this);
            if (child.getKey().equals(stopFieldInclusive)) {
                break;
            }
        }
    }

    public void visit(CobolChoiceType type) {
        maxBytesLen += type.getMaxBytesLen();
    }

    public void visit(CobolArrayType type) {
        maxBytesLen += type.getMaxBytesLen();
    }

    public void visit(CobolPrimitiveType < ? > type) {
        maxBytesLen += type.getMaxBytesLen();
    }

    public long getMaxBytesLen() {
        return maxBytesLen;
    }

}
