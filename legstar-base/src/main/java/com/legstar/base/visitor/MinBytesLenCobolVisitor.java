package com.legstar.base.visitor;

import java.util.Map.Entry;

import com.legstar.base.ConversionException;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

/**
 * Calculates the minimum number of bytes needed for a mainframe buffer to hold
 * a representation of a complex type.
 * <p/>
 * Variable size arrays are considered to have no occurrence.
 * <p/>
 * Choices are assumed to have the smallest alternative selected.
 * <p/>
 * Optionally calculation can stop on a field in the middle of the structure
 * (calculates partial length).
 * 
 */
public class MinBytesLenCobolVisitor implements CobolVisitor {

    long minBytesLen = 0;

    /**
     * Optionally indicates on which field to stop calculation
     */
    private final String stopFieldInclusive;

    public MinBytesLenCobolVisitor() {
        this(null);
    }

    public MinBytesLenCobolVisitor(String stopFieldInclusive) {
        this.stopFieldInclusive = stopFieldInclusive;
    }

    public void visit(CobolComplexType type) throws ConversionException {
        for (Entry < String, CobolType > child : type.getFields().entrySet()) {
            child.getValue().accept(this);
            if (child.getKey().equals(stopFieldInclusive)) {
                break;
            }
        }
    }

    public void visit(CobolChoiceType type) throws ConversionException {
        minBytesLen += type.getMinBytesLen();
    }

    public void visit(CobolArrayType type) throws ConversionException {
        minBytesLen += type.getMinBytesLen();
    }

    public void visit(CobolPrimitiveType < ? > type) throws ConversionException {
        minBytesLen += type.getMinBytesLen();
    }

    public long getMinBytesLen() {
        return minBytesLen;
    }

}
