package com.legstar.base.visitor;

import java.util.Map.Entry;

import com.legstar.base.ConversionException;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

/**
 * Calculates the maximum number of bytes needed for a mainframe buffer to hold
 * a representation of a complex type.
 * <p/>
 * Variable size arrays are considered to have all occurrences.
 * <p/>
 * Choices are assumed to have the largest alternative selected.
 * <p/>
 * Optionally calculation can stop on a field in the middle of the structure
 * (calculates partial length).
 * 
 * @return the maximum number of bytes for a corresponding mainframe buffer
 */
public class MaxBytesLenCobolVisitor implements CobolVisitor {

    int maxBytesLen = 0;

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

    public void visit(CobolComplexType type) throws ConversionException {
        for (Entry < String, CobolType > child : type.getFields().entrySet()) {
            child.getValue().accept(this);
            if (child.getKey().equals(stopFieldInclusive)) {
                break;
            }
        }
    }

    public void visit(CobolChoiceType type) throws ConversionException {

        int altMaxBytesLen = maxBytesLen;
        int savedMaxBytesLen = maxBytesLen;

        for (Entry < String, CobolType > alternative : type.getAlternatives()
                .entrySet()) {

            // What if this alternative is selected?
            alternative.getValue().accept(this);
            if (maxBytesLen > altMaxBytesLen) {
                // So far this is the largest alternative
                altMaxBytesLen = maxBytesLen;
            }
            // Restore counter before trying the next alternative
            maxBytesLen = savedMaxBytesLen;
        }

        // Final result is the largest alternative
        maxBytesLen = altMaxBytesLen;
    }

    public void visit(CobolArrayType type) throws ConversionException {

        // In COBOL all items have the same size so all we need is to calculate
        // one item size

        int savedMaxBytesLen = maxBytesLen;
        type.getItemType().accept(this);
        maxBytesLen = savedMaxBytesLen + type.getMaxOccurs()
                * (maxBytesLen - savedMaxBytesLen);
    }

    public void visit(CobolPrimitiveType < ? > type) throws ConversionException {
        maxBytesLen += type.getBytesLen();
    }

    public int getMaxBytesLen() {
        return maxBytesLen;
    }

}
