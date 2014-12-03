package com.legstar.base.visitor;

/**
 * A complex type name does not correspond to a known type.
 * 
 */
public class InvalidComplexTypeName extends RuntimeException {

    private static final long serialVersionUID = 4574129760170556492L;

    public InvalidComplexTypeName(String complexTypeName) {
        super("Name " + complexTypeName
                + " does not correspond to a complex type");
    }

}
