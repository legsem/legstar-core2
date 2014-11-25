package com.legsem.legstar.type.composite;

import java.util.LinkedHashMap;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.visitor.CobolVisitor;

/**
 * A COBOl Group item.
 * 
 */
public class CobolComplexType extends CobolCompositeType {

    /**
     * Children are kept in a LinkedHashMap to preserve entry order.
     */
    private final LinkedHashMap < String, CobolType > children;

    public CobolComplexType(CobolContext cobolContext,
            LinkedHashMap < String, CobolType > children) {
        super(cobolContext);
        this.children = children;
    }

    public LinkedHashMap < String, CobolType > getChildren() {
        return children;
    }

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

}
