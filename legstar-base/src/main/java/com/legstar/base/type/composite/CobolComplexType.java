package com.legstar.base.type.composite;

import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.visitor.CobolVisitor;

/**
 * A COBOl Group item.
 * 
 */
public class CobolComplexType extends CobolCompositeType {
    
    /**
     * A unique name for this complex type.
     */
    private final String name;

    /**
     * List of fields mapping to their COBOL type.
     */
    private final Map < String, CobolType > fields;

    public CobolComplexType(String name, Map < String, CobolType > children) {
        this.name = name;
        this.fields = children;
    }

    public Map < String, CobolType > getFields() {
        return fields;
    }

    public String getName() {
        return name;
    }

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

}
