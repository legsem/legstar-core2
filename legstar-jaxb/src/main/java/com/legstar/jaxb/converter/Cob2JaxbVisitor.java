package com.legstar.jaxb.converter;

import java.util.ArrayList;
import java.util.List;

import com.legstar.base.ConversionException;
import com.legstar.base.FromHostException;
import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;
import com.legstar.base.visitor.FromCobolChoiceStrategy;
import com.legstar.base.visitor.FromCobolVisitor;

/**
 * Given mainframe data this converter produces JAXB instances valued using the
 * converted mainframe data.
 * <p/>
 * Relies on {@link JaxbWrapperFactory} class to create actual JAXB instances
 * and {@link JaxbWrapper} classes to map COBOL fields to JAXB properties.
 * 
 */
public class Cob2JaxbVisitor extends FromCobolVisitor {

    private final JaxbPrimitiveTypeHandler primitiveTypeHandler;
    private final JaxbChoiceTypeAlternativeHandler choiceTypeAlternativeHandler;

    private final JaxbWrapperFactory jaxbWrapperFactory;

    /** Last java object produced by visiting an item. */
    private Object lastObject;

    /**
     * When a choice is encountered and an alternative is selected, this gives
     * the index of the chosen alternative in its parent choice.
     */
    private int lastAlternativeIndex = -1;

    // -----------------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------------
    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, JaxbWrapperFactory jaxbWrapperFactory) {
        this(cobolContext, hostData, start, jaxbWrapperFactory, null);
    }

    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, JaxbWrapperFactory jaxbWrapperFactory,
            FromCobolChoiceStrategy customChoiceStrategy) {
        this(cobolContext, hostData, start, jaxbWrapperFactory,
                customChoiceStrategy, null);
    }

    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, JaxbWrapperFactory jaxbWrapperFactory,
            FromCobolChoiceStrategy customChoiceStrategy,
            List < String > customVariables) {
        super(cobolContext, hostData, start, customChoiceStrategy,
                customVariables);
        primitiveTypeHandler = new JaxbPrimitiveTypeHandler();
        choiceTypeAlternativeHandler = new JaxbChoiceTypeAlternativeHandler();
        this.jaxbWrapperFactory = jaxbWrapperFactory;
    }

    // -----------------------------------------------------------------------------
    // Visit methods
    // -----------------------------------------------------------------------------
    public void visit(CobolComplexType type) throws ConversionException {
        final JaxbWrapper < ? > complexJaxbWrapper = jaxbWrapperFactory
                .create(type);
        super.visitComplexType(type, new JaxbComplexTypeChildHandler(
                complexJaxbWrapper));
        lastObject = complexJaxbWrapper;
    }

    public void visit(CobolArrayType type) throws ConversionException {
        final List < Object > list = new ArrayList < Object >();
        super.visitCobolArrayType(type, new JaxbArrayTypeItemHandler(list));
        lastObject = list;
    }

    public void visit(CobolChoiceType type) throws ConversionException {
        super.visitCobolChoiceType(type, choiceTypeAlternativeHandler);
    }

    public void visit(CobolPrimitiveType < ? > type) throws ConversionException {
        super.visitCobolPrimitiveType(type, primitiveTypeHandler);
    }

    // -----------------------------------------------------------------------------
    // Handlers
    // -----------------------------------------------------------------------------
    private class JaxbComplexTypeChildHandler implements
            ComplexTypeChildHandler {

        private final JaxbWrapper < ? > complexJaxbWrapper;

        public JaxbComplexTypeChildHandler(JaxbWrapper < ? > complexJaxbWrapper) {
            this.complexJaxbWrapper = complexJaxbWrapper;
        }

        public boolean postVisit(String fieldName, int fieldIndex,
                CobolType child) {
            complexJaxbWrapper.set(fieldIndex, lastObject, lastAlternativeIndex);
            lastAlternativeIndex = -1;
            return true;
        }
    }

    private class JaxbArrayTypeItemHandler implements ArrayTypeItemHandler {

        private final List < Object > list;

        public JaxbArrayTypeItemHandler(List < Object > list) {
            this.list = list;
        }

        public boolean postVisit(int itemIndex, CobolType item) {
            list.add(lastObject);
            return true;
        }

    }

    private class JaxbChoiceTypeAlternativeHandler implements
            ChoiceTypeAlternativeHandler {

        public void postVisit(String alternativeName, int alternativeIndex,
                CobolType alternative) {
            lastAlternativeIndex = alternativeIndex;
        }

    }

    private class JaxbPrimitiveTypeHandler implements PrimitiveTypeHandler {

        public void postVisit(CobolType type, Object value) {
            lastObject = value;
        }

    }

    // -----------------------------------------------------------------------------
    // Getters
    // -----------------------------------------------------------------------------
    public Object getLastObject() {
        return lastObject;
    }

    public <T> T getLastObject(Class < T > clazz) {
        if (clazz.isAssignableFrom(lastObject.getClass())) {
            return clazz.cast(lastObject);
        } else {
            throw new FromHostException("Object of class "
                    + lastObject.getClass() + " is not assignable from "
                    + clazz, getHostData(), getLastPos());
        }
    };

}
