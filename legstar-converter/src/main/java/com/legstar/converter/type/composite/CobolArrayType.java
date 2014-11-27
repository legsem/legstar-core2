package com.legstar.converter.type.composite;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.utils.StringUtils;
import com.legstar.converter.visitor.CobolVisitor;

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

    private final int maxOccurs;

    private final String dependingOn;

    public CobolArrayType(CobolType itemType, int maxOccurs) {
        this(itemType, maxOccurs, null);
    }

    public CobolArrayType(CobolType itemType, int maxOccurs, String dependingOn) {
        this.maxOccurs = maxOccurs;
        this.itemType = itemType;
        this.dependingOn = dependingOn;
    }

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public CobolType getItemType() {
        return itemType;
    }

    public int getMaxOccurs() {
        return maxOccurs;
    }

    public String getDependingOn() {
        return dependingOn;
    }

    public boolean isVariableSize() {
        return StringUtils.isNotBlank(dependingOn);
    }

}
