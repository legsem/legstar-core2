package com.legstar.jaxb.converter;


import com.legstar.base.converter.AbstractCob2ObjectConverter;
import com.legstar.base.converter.FromHostResult;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.jaxb.converter.Cob2JaxbVisitor;
import com.legstar.jaxb.converter.JaxbWrapper;

/**
 * Converts mainframe data to a JAXB instance.
 * <p/>
 * Given a buffer of mainframe data corresponding to a COBOL copybook.
 * <p/>
 * Uses a {@link CobolComplexType} to visit all fields of the COBOL copybook.
 * <p/>
 * Uses a {@link JaxbWrapperFactory} to create resulting JAXB instances.
 *
 * @param <J> the target JAXB object type
 */
public class Cob2JaxbConverter<J> extends AbstractCob2ObjectConverter < J > {

    /**
     * Provides methods to create and populate the target JAXB object
     */
    private final JaxbWrapperFactory jaxbWrapperFactory;

    /**
     * The target JAXB instance class
     */
    private final Class < J > jaxbClass;

    /**
     * Convert host data to a JAXB instance.
     * <p/>
     * 
     * @param hostData the buffer of host data
     * @param start where to start in the buffer of host data
     * @return a result object containing the JAXB instance created as well as
     *         the total number of host bytes used to generate that JAXB
     *         instance
     */
    public FromHostResult < J > convert(byte[] hostData, int start) {
        Cob2JaxbVisitor visitor = new Cob2JaxbVisitor(getCobolContext(),
                hostData, start, jaxbWrapperFactory);
        visitor.visit(getCobolComplexType());
        JaxbWrapper < ? > jaxbWrapper = visitor
                .getLastObject(JaxbWrapper.class);
        return new FromHostResult < J >(visitor.getLastPos(),
                jaxbClass.cast(jaxbWrapper.getJaxb()));
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder<J> extends
            AbstractCob2ObjectConverter.Builder < J, Builder < J > > {

        private JaxbWrapperFactory jaxbWrapperFactory;
        private Class < J > jaxbClass;

        public Builder < J > jaxbWrapperFactory(
                JaxbWrapperFactory jaxbWrapperFactory) {
            this.jaxbWrapperFactory = jaxbWrapperFactory;
            return this;
        }

        public Builder < J > jaxbClass(Class < J > jaxbClass) {
            this.jaxbClass = jaxbClass;
            return this;
        }

        public Cob2JaxbConverter < J > build() {
            return new Cob2JaxbConverter < J >(this);
        }

        protected Builder < J > self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    public Cob2JaxbConverter(Builder < J > builder) {
        super(builder);
        jaxbWrapperFactory = builder.jaxbWrapperFactory;
        jaxbClass = builder.jaxbClass;
        if (jaxbWrapperFactory == null) {
            throw new IllegalArgumentException(
                    "You must provide a valid input JAXB wrapper factory");
        }
        if (jaxbClass == null) {
            throw new IllegalArgumentException(
                    "You must provide a valid input JAXB class");
        }
    }
}
