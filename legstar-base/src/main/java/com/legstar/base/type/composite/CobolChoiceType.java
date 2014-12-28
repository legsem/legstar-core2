package com.legstar.base.type.composite;

import java.util.Map;
import java.util.Map.Entry;

import com.legstar.base.type.CobolType;
import com.legstar.base.visitor.CobolVisitor;

/**
 * A COBOL REDEFINES clause translates into a choice between alternatives (which
 * could be primitive or composite types).
 * 
 */
public class CobolChoiceType extends CobolCompositeType {

    /**
     * A unique name for this choice.
     */
    private final String name;

    /**
     * List of alternatives mapping to their COBOL type.
     */
    private final Map < String, CobolType > alternatives;

    /**
     * Maximum size in bytes.
     */
    private final int maxBytesLen;

    /**
     * Builds a choice from a series of named alternatives.
     * 
     * @param name a unique name for this choice
     * @param alternatives the mapping of alternatives to their names. It is
     *            important that this structure preserves insertion order (such
     *            as {@link java.util.LinkedHashMap} )
     */
    public CobolChoiceType(String name, Map < String, CobolType > alternatives) {
        this.name = name;
        this.alternatives = alternatives;
        int maxBytesLen = 0;
        for (CobolType alternative : alternatives.values()) {
            maxBytesLen = maxBytesLen < alternative.getMaxBytesLen() ? alternative
                    .getMaxBytesLen() : maxBytesLen;
        }
        this.maxBytesLen = maxBytesLen;
    }

    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public String getName() {
        return name;
    }

    public Map < String, CobolType > getAlternatives() {
        return alternatives;
    }

    public String getAlternativeName(CobolType alternative) {
        for (Entry < String, CobolType > entry : alternatives.entrySet()) {
            if (entry.getValue().equals(alternative)) {
                return entry.getKey();
            }
        }
        return null;
    }

    public int getAlternativeIndex(CobolType alternative) {
        int index = 0;
        for (Entry < String, CobolType > entry : alternatives.entrySet()) {
            if (entry.getValue().equals(alternative)) {
                return index;
            }
            index++;
        }
        return -1;
    }

    public int getAlternativeIndex(String alternativeName) {
        int index = 0;
        for (Entry < String, CobolType > entry : alternatives.entrySet()) {
            if (entry.getKey().equals(alternativeName)) {
                return index;
            }
            index++;
        }
        return -1;
    }

    /** {@inheritDoc} */
    public int getMaxBytesLen() {
        return maxBytesLen;
    }
}
