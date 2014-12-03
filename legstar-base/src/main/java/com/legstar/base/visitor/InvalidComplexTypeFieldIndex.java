package com.legstar.base.visitor;

/**
 * An index does not correspond to any field in a parent complex type.
 * 
 */
public class InvalidComplexTypeFieldIndex extends RuntimeException {

    private static final long serialVersionUID = -1958469508640741601L;

    public InvalidComplexTypeFieldIndex(String complexTypeName, int index) {
        super("Index " + index + " does not correspond to one of "
                + complexTypeName + " fields");
    }

}
