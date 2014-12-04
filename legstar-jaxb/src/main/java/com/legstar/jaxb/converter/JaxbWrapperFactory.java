package com.legstar.jaxb.converter;

import com.legstar.base.type.composite.CobolComplexType;

/**
 * Creates {@link JaxbWrapper} classes for a corresponding COBOL complex type.
 * 
 */
public interface JaxbWrapperFactory {

    /**
     * Given a COBOL complex type, create a corresponding wrapper on a new JAXB
     * instance.
     * 
     * @param type the COBOL complex type
     * @return a corresponding JAXB wrapper
     */
    JaxbWrapper < ? > create(CobolComplexType type);

    /**
     * Given a COBOL complex type, create a corresponding wrapper on an existing JAXB
     * instance.
     * 
     * @param type the COBOL complex type
     * @param jaxb the JAXB instance to wrap
     * @return a corresponding JAXB wrapper
     */
    JaxbWrapper < ? > create(CobolComplexType type, Object jaxb);

}
