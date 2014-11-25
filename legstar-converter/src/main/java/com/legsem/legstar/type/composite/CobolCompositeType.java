package com.legsem.legstar.type.composite;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;

/**
 * Composite types are built from other types.
 *
 */
public abstract class CobolCompositeType extends CobolType {

    public CobolCompositeType(CobolContext cobolContext) {
        super(cobolContext);
    }

}
