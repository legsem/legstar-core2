package com.legstar.jaxb.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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
 * <p>
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
            JaxbWrapperFactory jaxbWrapperFactory) {
        this(cobolContext, hostData, 0, hostData.length, jaxbWrapperFactory,
                null);
    }

    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, int length, JaxbWrapperFactory jaxbWrapperFactory) {
        this(cobolContext, hostData, start, length, jaxbWrapperFactory, null);
    }

    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, int length, JaxbWrapperFactory jaxbWrapperFactory,
            FromCobolChoiceStrategy customChoiceStrategy) {
        this(cobolContext, hostData, start, length, jaxbWrapperFactory,
                customChoiceStrategy, null);
    }

    public Cob2JaxbVisitor(CobolContext cobolContext, byte[] hostData,
            int start, int length, JaxbWrapperFactory jaxbWrapperFactory,
            FromCobolChoiceStrategy customChoiceStrategy,
            Set < String > customVariables) {
        super(cobolContext, hostData, start, length, customChoiceStrategy,
                customVariables);
        primitiveTypeHandler = new JaxbPrimitiveTypeHandler();
        choiceTypeAlternativeHandler = new JaxbChoiceTypeAlternativeHandler();
        this.jaxbWrapperFactory = jaxbWrapperFactory;
    }

    // -----------------------------------------------------------------------------
    // Visit methods
    // -----------------------------------------------------------------------------
    public void visit(CobolComplexType type) {
        final JaxbWrapper < ? > complexJaxbWrapper = jaxbWrapperFactory
                .create(type);
        super.visitComplexType(type, new JaxbComplexTypeChildHandler(
                complexJaxbWrapper));
        lastObject = complexJaxbWrapper;
    }

    public void visit(CobolArrayType type) {
        final List < Object > list = new ArrayList < Object >();
        super.visitCobolArrayType(type, new JaxbArrayTypeItemHandler(list));
        lastObject = list;
    }

    public void visit(CobolChoiceType type) {
        super.visitCobolChoiceType(type, choiceTypeAlternativeHandler);
    }

    public void visit(CobolPrimitiveType < ? > type) {
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

        public boolean preVisit(String fieldName, int fieldIndex,
                CobolType child) {
            return true;
        }

        public boolean postVisit(String fieldName, int fieldIndex,
                CobolType child) {
            complexJaxbWrapper
                    .set(fieldIndex, lastObject, lastAlternativeIndex);
            lastAlternativeIndex = -1;
            return true;
        }
    }

    private class JaxbArrayTypeItemHandler implements ArrayTypeItemHandler {

        private final List < Object > list;

        public JaxbArrayTypeItemHandler(List < Object > list) {
            this.list = list;
        }

        public boolean preVisit(int itemIndex, CobolType item) {
            return true;
        }

        public boolean postVisit(int itemIndex, CobolType item) {
            list.add(lastObject);
            return true;
        }

    }

    private class JaxbChoiceTypeAlternativeHandler implements
            ChoiceTypeAlternativeHandler {

        public void preVisit(String alternativeName, int alternativeIndex,
                CobolType alternative) {
        }

        public void postVisit(String alternativeName, int alternativeIndex,
                CobolType alternative) {
            lastAlternativeIndex = alternativeIndex;
        }

    }

    private class JaxbPrimitiveTypeHandler implements PrimitiveTypeHandler {

        public void preVisit(CobolPrimitiveType < ? > type) {

        }

        public void postVisit(CobolPrimitiveType < ? > type, Object value) {
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
            throw new IllegalArgumentException("Object of class "
                    + lastObject.getClass() + " is not assignable from "
                    + clazz);
        }
    };

}
