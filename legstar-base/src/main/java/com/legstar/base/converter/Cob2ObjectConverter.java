package com.legstar.base.converter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.legstar.base.ConversionException;
import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;
import com.legstar.base.visitor.FromCobolChoiceStrategy;
import com.legstar.base.visitor.FromCobolVisitor;

/**
 * Convert a mainframe data to a java Object.
 * <p/>
 * COBOL Complex types are converted to java Maps.
 * 
 */
public class Cob2ObjectConverter extends FromCobolVisitor {

    /**
     * Set of unique handlers to receive notifications from
     * {@link FromCobolVisitor}
     */
    private final ObjectPrimitiveTypeHandler primitiveTypeHandler;
    private final ObjectChoiceTypeAlternativeHandler choiceTypeAlternativeHandler;

    /** Last java object produced by visiting an item. */
    private Object resultObject;

    // -----------------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------------
    public Cob2ObjectConverter(CobolContext cobolContext, byte[] hostData,
            int start) {
        this(cobolContext, hostData, start, null);
    }

    public Cob2ObjectConverter(CobolContext cobolContext, byte[] hostData,
            int start, FromCobolChoiceStrategy customChoiceStrategy) {
        this(cobolContext, hostData, start, customChoiceStrategy, null);
    }

    public Cob2ObjectConverter(CobolContext cobolContext, byte[] hostData,
            int start, FromCobolChoiceStrategy customChoiceStrategy, Set < String > customVariables) {
        super(cobolContext, hostData, start, customChoiceStrategy, customVariables);
        primitiveTypeHandler = new ObjectPrimitiveTypeHandler();
        choiceTypeAlternativeHandler = new ObjectChoiceTypeAlternativeHandler();
    }

    // -----------------------------------------------------------------------------
    // Visit methods
    // -----------------------------------------------------------------------------
    public void visit(CobolComplexType type) throws ConversionException {
        final Map < String, Object > map = new LinkedHashMap < String, Object >();
        super.visitComplexType(type, new ObjectComplexTypeChildHandler(map));
        resultObject = map;
    }

    public void visit(CobolArrayType type) throws ConversionException {
        final List < Object > list = new ArrayList < Object >();
        super.visitCobolArrayType(type, new ObjectArrayTypeItemHandler(list));
        resultObject = list;
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
    private class ObjectComplexTypeChildHandler implements
            ComplexTypeChildHandler {

        private final Map < String, Object > map;

        public ObjectComplexTypeChildHandler(Map < String, Object > map) {
            this.map = map;
        }

        public boolean preVisit(String fieldName, int fieldIndex,
                CobolType child) {
            return true;
        }

        public boolean postVisit(String fieldName, int fieldIndex, CobolType child) {
            map.put(fieldName, resultObject);
            return true;
        }

    }

    private class ObjectArrayTypeItemHandler implements ArrayTypeItemHandler {

        private final List < Object > list;

        public ObjectArrayTypeItemHandler(List < Object > list) {
            this.list = list;
        }

        public boolean preVisit(int itemIndex, CobolType item) {
            return true;
        }

        public boolean postVisit(int itemIndex, CobolType item) {
            list.add(resultObject);
            return true;
        }

    }

    private class ObjectChoiceTypeAlternativeHandler implements
            ChoiceTypeAlternativeHandler {

        public void preVisit(String alternativeName, int alternativeIndex,
                CobolType alternative) {
        }

        public void postVisit(String alternativeName, int alternativeIndex, CobolType alternative) {
            // Wrap the chosen alternative in a map
            final Map < String, Object > map = new LinkedHashMap < String, Object >();
            map.put(alternativeName, resultObject);
            resultObject = map;
        }

    }

    private class ObjectPrimitiveTypeHandler implements PrimitiveTypeHandler {

        public void preVisit(CobolPrimitiveType<?> type) {
            
        }

        public void postVisit(CobolPrimitiveType<?> type, Object value) {
            resultObject = value;
        }

    };

    // -----------------------------------------------------------------------------
    // Getters
    // -----------------------------------------------------------------------------
    public Object getResultObject() {
        return resultObject;
    }

}
