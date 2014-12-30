package com.legstar.base.type;

/**
 * Implemented by types which may or may not be present depending on some other field.
 *
 */
public interface CobolOptionalType {
    
    /**
     * Name of the field that conditions the existence of this type.
     * @return
     */
    String getDependingOn();

}
