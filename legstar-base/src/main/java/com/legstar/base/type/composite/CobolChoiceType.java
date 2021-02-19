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
    private final long maxBytesLen;

    /**
     * Minimum size in bytes.
     */
    private final long minBytesLen;

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
    public long getMinBytesLen() {
        return minBytesLen;
    }

    /** {@inheritDoc} */
    public long getMaxBytesLen() {
        return maxBytesLen;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Choices are wrappers without a name in COBOL.
     */
    public String getCobolName() {
        return null;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder {

        private String name;
        private Map < String, CobolType > alternatives;

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder alternatives(Map < String, CobolType > alternatives) {
            this.alternatives = alternatives;
            return this;
        }

        public CobolChoiceType build() {
            return new CobolChoiceType(this);
        }

        protected Builder self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolChoiceType(Builder builder) {

        name = builder.name;
        alternatives = builder.alternatives;
        long minBl = Long.MAX_VALUE;
        long maxBl = 0;
        for (CobolType alternative : alternatives.values()) {
            maxBl = maxBl < alternative.getMaxBytesLen() ? alternative
                    .getMaxBytesLen() : maxBl;
            minBl = minBl > alternative.getMinBytesLen() ? alternative
                    .getMinBytesLen() : minBl;
        }
        maxBytesLen = maxBl;
        minBytesLen = minBl;
    }

}
