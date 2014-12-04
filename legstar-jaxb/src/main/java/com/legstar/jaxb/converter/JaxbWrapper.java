package com.legstar.jaxb.converter;

/**
 * A wrapper on a JAXB instance.
 * <p/>
 * Allows getting/setting the underlying JAXB instance properties using symbolic
 * field names or position (Order in which properties are declared in the JAXB
 * instance).
 * 
 */
public abstract class JaxbWrapper<J> {

    private final J jaxb;

    public JaxbWrapper(J jaxb) {
        this.jaxb = jaxb;
    }

    /**
     * Set the JAXB property with a new mainframe value.
     * 
     * @param index the position of this field in its parent complex type
     * @param value the value to assign to that property
     */
    public abstract void set(int index, Object value);

    /**
     * Get the JAXB property value.
     * 
     * @param index the position of this field in its parent complex type
     * @return the JAXB property value
     */
    public abstract Object get(int index);

    public J getJaxb() {
        return jaxb;
    }

}
