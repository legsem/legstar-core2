package com.legstar.base.type.composite;

import com.legstar.base.type.CobolType;
import com.legstar.base.visitor.CobolVisitor;

/**
 * Array of other types.
 * <p/>
 * If there is no dependingOn then this is a fixed size array otherwise, it is a
 * variable size array which size is determined at runtime by the value of the
 * dependingOn variable.
 * 
 * @param an item java object type
 */
public class CobolArrayType extends CobolCompositeType {

    private final CobolType itemType;

    private final int minOccurs;

    private final int maxOccurs;

    private final String dependingOn;

    /**
     * Minimum size in bytes.
     */
    private final long minBytesLen;

    /**
     * Maximum size in bytes.
     */
    private final long maxBytesLen;

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public CobolType getItemType() {
        return itemType;
    }

    public int getMinOccurs() {
        return minOccurs;
    }

    public int getMaxOccurs() {
        return maxOccurs;
    }

    public String getDependingOn() {
        return dependingOn;
    }

    public boolean isVariableSize() {
        return maxOccurs > minOccurs;
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
     *  {@inheritDoc} 
     *  <p/>
     *  Arrays are wrappers without a name in COBOL.
     */
    public String getCobolName() {
        return null;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder {

        private CobolType itemType;
        private int minOccurs;
        private int maxOccurs;
        private String dependingOn;

        public Builder itemType(CobolType itemType) {
            this.itemType = itemType;
            return this;
        }

        public Builder minOccurs(int minOccurs) {
            this.minOccurs = minOccurs;
            return this;
        }

        public Builder maxOccurs(int maxOccurs) {
            this.maxOccurs = maxOccurs;
            return this;
        }

        public Builder dependingOn(String dependingOn) {
            this.dependingOn = dependingOn;
            return this;
        }

        public CobolArrayType build() {
            return new CobolArrayType(this);
        }

        protected Builder self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private CobolArrayType(Builder builder) {

        itemType = builder.itemType;
        minOccurs = builder.minOccurs;
        maxOccurs = builder.maxOccurs;
        dependingOn = builder.dependingOn;
        minBytesLen = minOccurs * itemType.getMinBytesLen();
        maxBytesLen = maxOccurs * itemType.getMaxBytesLen();
    }

}
