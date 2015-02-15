package com.legstar.base.type.composite;

import java.util.Map;

import com.legstar.base.type.CobolOptionalType;
import com.legstar.base.type.CobolType;
import com.legstar.base.visitor.CobolVisitor;

/**
 * A COBOl Group item.
 * 
 */
public class CobolComplexType extends CobolCompositeType implements CobolOptionalType {
    
    /**
     * A unique name for this complex type.
     */
    private final String name;

    /**
     * List of fields mapping to their COBOL type.
     */
    private final Map < String, CobolType > fields;
    
    /**
     * Optional types may depend on some other field.
     */
    private final String dependingOn;

    /**
     * Minimum size in bytes.
     */
    private final long minBytesLen;

    /**
     * Maximum size in bytes.
     */
    private final long maxBytesLen;

    public Map < String, CobolType > getFields() {
        return fields;
    }

    public String getName() {
        return name;
    }

    public String getDependingOn() {
        return dependingOn;
    }

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    /** {@inheritDoc} */
    public long getMinBytesLen() {
        return minBytesLen;
    }

    /** {@inheritDoc} */
    public long getMaxBytesLen() {
        return maxBytesLen;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder {

        private String name;
        private Map < String, CobolType > fields;
        private String dependingOn;

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder fields(Map < String, CobolType > fields) {
            this.fields = fields;
            return this;
        }

        public Builder dependingOn(String dependingOn) {
            this.dependingOn = dependingOn;
            return this;
        }

        public CobolComplexType build() {
            return new CobolComplexType(this);
        }

        protected Builder self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    public CobolComplexType(Builder builder) {

        name = builder.name;
        fields = builder.fields;
        dependingOn = builder.dependingOn;
        long minBl = 0;
        long maxBl = 0;
        for (CobolType child : fields.values()) {
            minBl += child.getMinBytesLen();
            maxBl += child.getMaxBytesLen();
        }
        minBytesLen = minBl;
        maxBytesLen = maxBl;
    }
}
