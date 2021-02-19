package com.legstar.base.converter;

import java.util.Map;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.Cob2ObjectVisitor;

/**
 * Converts mainframe data to a hash map.
 * <p>
 * Convenience class using {@link Cob2ObjectVisitor} to perform a record
 * conversion.
 * <p>
 * Assumes a {@link CobolComplexType} is available. The CobolComplexType
 * describes the incoming mainframe datum.
 * <p>
 * This class is immutable and Thread safe.
 * 
 */
public class Cob2HashMapConverter extends
        AbstractCob2ObjectConverter < Map < String, Object >> {

    @SuppressWarnings("unchecked")
    public FromHostResult < Map < String, Object > > convert(byte[] hostData,
            int start, int length) {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(getCobolContext(),
                hostData, start, length, getCustomChoiceStrategy(),
                getCustomVariables());
        visitor.visit(getCobolComplexType());
        return new FromHostResult < Map < String, Object > >(
                visitor.getLastPos(),
                (Map < String, Object >) visitor.getResultObject());
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public static class Builder
            extends
            AbstractCob2ObjectConverter.Builder < Map < String, Object >, Builder > {

        public Cob2HashMapConverter build() {
            return new Cob2HashMapConverter(this);
        }

        protected Builder self() {
            return this;
        }

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    private Cob2HashMapConverter(Builder builder) {
        super(builder);
    }

}
