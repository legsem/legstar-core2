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

    int minBytesLen = 0;

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

        int altMinBytesLen = minBytesLen;
        int savedMinBytesLen = minBytesLen;

        for (Entry < String, CobolType > alternative : type.getAlternatives()
                .entrySet()) {

            // What if this alternative is selected?
            alternative.getValue().accept(this);
            
            if (altMinBytesLen == savedMinBytesLen) {
                // First alternative is necessarily the min so far
                altMinBytesLen = minBytesLen;
            } else {
                if (minBytesLen < altMinBytesLen) {
                    // From now on this is the smallest alternative
                    altMinBytesLen = minBytesLen;
                }
            }
            // Restore counter before trying the next alternative
            minBytesLen = savedMinBytesLen;
        }

        // Final result is the largest alternative
        minBytesLen = altMinBytesLen;
    }

    public void visit(CobolArrayType type) throws ConversionException {
        if (type.isVariableSize()) {
            return;
        }
        for (int i = 0; i < type.getMaxOccurs(); i++) {
            type.getItemType().accept(this);
        }
    }

    public void visit(CobolPrimitiveType < ? > type) throws ConversionException {
        minBytesLen += type.getBytesLen();
    }

    public int getMinBytesLen() {
        return minBytesLen;
    }

}
