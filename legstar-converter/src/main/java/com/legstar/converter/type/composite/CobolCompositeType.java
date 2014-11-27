package com.legstar.converter.type.composite;

import com.legstar.converter.context.CobolContext;
import com.legstar.converter.type.CobolType;

/**
 * Composite types are built from other types.
 *
 */
public abstract class CobolCompositeType extends CobolType {

    public CobolCompositeType(CobolContext cobolContext) {
        super(cobolContext);
    }

}
