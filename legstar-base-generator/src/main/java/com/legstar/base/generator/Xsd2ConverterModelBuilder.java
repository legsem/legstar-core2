package com.legstar.base.generator;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAll;
import org.apache.ws.commons.schema.XmlSchemaAllMember;
import org.apache.ws.commons.schema.XmlSchemaChoice;
import org.apache.ws.commons.schema.XmlSchemaChoiceMember;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaFacet;
import org.apache.ws.commons.schema.XmlSchemaMaxLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaParticle;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSequenceMember;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.constants.Constants;
import org.apache.ws.commons.schema.utils.XmlSchemaObjectBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cobol.model.CobolAnnotations;
import com.legstar.cobol.model.CobolTypes;

/**
 * Build a model using a COBOL-annotated XML schema such as the ones produced by
 * legstar-cob2xsd.
 * <p/>
 * The model is organized as a set of hierarchical properties which are easy to
 * use by a template engine.
 * 
 */
public class Xsd2ConverterModelBuilder {

    private static final String CHOICE_FIELD_NAME_SUFFIX = "Choice";

    /** Logging. */
    private static Logger log = LoggerFactory
            .getLogger(Xsd2ConverterModelBuilder.class);

    /**
     * Maps an Object Depending On COBOL name to its properties. This helps
     * enriching the odo object with target arrays characteristics.
     * <p/>
     */
    Map < String, Object > odoObjects = new HashMap < String, Object >();

    /**
     * Maps an Object Depending On COBOL name to its element name in the XML
     * schema.
     */
    Map < String, String > odoObjectNames = new HashMap < String, String >();

    /**
     * Process each element in the input Schema.
     * <p/>
     * 
     * @param xmlSchema the XML schema with COBOL annotations
     * @return a map of root elements in the XML schema, each one mapped to its
     *         composite types constituents
     * @throws Xsd2ConverterException if parsing the XML schema fails
     */
    public Map < String, CompositeTypes > build(XmlSchema xmlSchema)
            throws Xsd2ConverterException {

        log.debug("visit XML Schema started");
        Map < String, CompositeTypes > rootComplexTypes = new LinkedHashMap < String, CompositeTypes >();

        for (Entry < QName, XmlSchemaElement > entry : xmlSchema.getElements()
                .entrySet()) {
            if (entry.getValue().getSchemaType() instanceof XmlSchemaComplexType) {
                XmlSchemaComplexType xsdComplexType = (XmlSchemaComplexType) entry
                        .getValue().getSchemaType();
                CompositeTypes compositeTypes = new CompositeTypes();
                String complexTypeName = getComplexTypeName(xsdComplexType);
                rootComplexTypes.put(complexTypeName, compositeTypes);
                visit(xsdComplexType, compositeTypes, complexTypeName);
            }
        }

        log.debug("visit XML Schema ended");
        return rootComplexTypes;
    }

    /**
     * Gathers all composite types for a given root element in the schema.
     * <p/>
     * Composite types are complex types, choice types, arrays.
     * 
     * 
     */
    public class CompositeTypes {

        public final Map < String, Object > complexTypes = new LinkedHashMap < String, Object >();

        public final Map < String, Object > choiceTypes = new LinkedHashMap < String, Object >();

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("[complexTypes=");
            builder.append(complexTypes);
            if (choiceTypes.size() > 0) {
                builder.append(", choiceTypes=");
                builder.append(choiceTypes);
            }
            builder.append("]");
            return builder.toString();
        };
    }

    /**
     * Visit each child of a complex type in turn.
     * <p/>
     * Contribute a complex type to a list of complex types.
     * 
     * @param xsdComplexType the new complex type visited
     * @param compositeTypes the lists of composite types being populated
     * @param complexTypeName the name to use for this complex type
     */
    private void visit(XmlSchemaComplexType xsdComplexType,
            CompositeTypes compositeTypes, String complexTypeName) {

        Map < String, Object > fields = new LinkedHashMap < String, Object >();
        XmlSchemaParticle particle = xsdComplexType.getParticle();
        if (particle instanceof XmlSchemaSequence) {
            int fieldIndex = 0;
            XmlSchemaSequence sequence = (XmlSchemaSequence) particle;
            for (XmlSchemaSequenceMember member : sequence.getItems()) {
                addField(fieldIndex, member, fields, compositeTypes);
                fieldIndex++;
            }

        } else if (particle instanceof XmlSchemaAll) {
            int fieldIndex = 0;
            XmlSchemaAll all = (XmlSchemaAll) particle;
            for (XmlSchemaAllMember member : all.getItems()) {
                addField(fieldIndex, member, fields, compositeTypes);
                fieldIndex++;
            }

        } else {
            /* TODO process other particle types of interest */
            /* TODO find a way to handle xsd:attribute */
            log.warn("Schema object does not contain a sequence or all element at line "
                    + xsdComplexType.getLineNumber());
        }
        compositeTypes.complexTypes.put(complexTypeName, fields);

    }

    /**
     * Visit each alternative of a choice in turn.
     * <p/>
     * Note that this produces a new complex type.
     * 
     * @param xsdChoice the XML schema choice
     * @param compositeTypes the lists of composite types being populated
     * @param choiceTypeName the name to use for this choice type
     */
    private void visit(XmlSchemaChoice xsdChoice,
            CompositeTypes compositeTypes, String choiceTypeName) {
        Map < String, Object > alternatives = new LinkedHashMap < String, Object >();
        int fieldIndex = 0;
        for (XmlSchemaChoiceMember alternative : xsdChoice.getItems()) {
            if (alternative instanceof XmlSchemaElement) {
                addField(fieldIndex, alternative, alternatives, compositeTypes);
                fieldIndex++;
            }
        }
        compositeTypes.choiceTypes.put(choiceTypeName, alternatives);

    }

    /**
     * Add a field with associated properties to a complex type.
     * 
     * @param index the order of the field in the parent complex type
     * @param xsdSchemaObject the potential field
     * @param xsdSchemaObject the potential field
     * @param fields the parent complex type's fields collection
     * @param compositeTypes the lists of composite types being populated
     */
    private void addField(int fieldIndex, XmlSchemaObjectBase xsdSchemaObject,
            Map < String, Object > fields, CompositeTypes compositeTypes) {
        if (xsdSchemaObject instanceof XmlSchemaElement) {
            XmlSchemaElement xsdElement = (XmlSchemaElement) xsdSchemaObject;
            fields.put(getFieldName(xsdElement),
                    getProps(fieldIndex, xsdElement, compositeTypes));
        } else if (xsdSchemaObject instanceof XmlSchemaChoice) {
            XmlSchemaChoice xsdChoice = (XmlSchemaChoice) xsdSchemaObject;
            fields.put(getFieldName(xsdChoice),
                    getProps(fieldIndex, xsdChoice, compositeTypes));
        }
        // TODO Add Groups
    }

    /**
     * Retrieve the properties of a choice element.
     * <p/>
     * Use the opportunity to visit each of the choice's alternatives.
     * 
     * @param fieldIndex the order of the choice in the parent complex type
     * @param xsdChoice the choice element
     * @param compositeTypes the lists of composite types being populated
     * @return the choice's properties
     */
    private Map < String, Object > getProps(int fieldIndex,
            XmlSchemaChoice xsdChoice, CompositeTypes compositeTypes) {

        String choiceTypeName = getComplexTypeName(xsdChoice);
        visit(xsdChoice, compositeTypes, choiceTypeName);

        Map < String, Object > props = new HashMap < String, Object >();
        props.put("fieldIndex", fieldIndex);
        props.put("choiceType", true);
        props.put("choiceTypeName", choiceTypeName);
        props.put("alternatives", compositeTypes.choiceTypes.get(choiceTypeName));

        return props;

    }

    /**
     * Retrieve the properties of a complex type.
     * <p/>
     * Use the opportunity to visit the complex typew type's children.
     * 
     * @param xsdComplexType the xsd complex type
     * @param compositeTypes the lists of composite types being populated
     * @return the complex type properties
     */
    private Map < String, Object > getProps(
            XmlSchemaComplexType xsdComplexType, CompositeTypes compositeTypes) {

        String complexTypeName = getComplexTypeName(xsdComplexType);
        visit(xsdComplexType, compositeTypes, complexTypeName);

        Map < String, Object > props = new HashMap < String, Object >();
        props.put("complexType", true);
        props.put("complexTypeName", complexTypeName);

        return props;

    }

    private Map < String, Object > getProps(int fieldIndex,
            XmlSchemaElement xsdElement, CompositeTypes compositeTypes) {
        Map < String, Object > props;
        CobolAnnotations cobolAnnotations = new CobolAnnotations(xsdElement);

        if (xsdElement.getSchemaType() instanceof XmlSchemaComplexType) {
            props = getProps((XmlSchemaComplexType) xsdElement.getSchemaType(),
                    compositeTypes);

        } else if (xsdElement.getSchemaType() instanceof XmlSchemaSimpleType) {
            props = getProps((XmlSchemaSimpleType) xsdElement.getSchemaType(),
                    cobolAnnotations);
            if (props.get("odoObject") != null) {
                odoObjectNames.put(cobolAnnotations.getCobolName(),
                        getFieldName(xsdElement));
            }

        } else {
            throw new Xsd2ConverterException("Unsupported xsd element of type "
                    + xsdElement.getSchemaType().getQName() + " at line "
                    + xsdElement.getLineNumber());
        }

        if (xsdElement.getMaxOccurs() > 1) {
            addArrayProps(xsdElement, cobolAnnotations, props);
        }

        props.put("fieldIndex", fieldIndex);
        return props;

    }

    /**
     * A complex type name is derived from an XML schema complex type name.
     * 
     * @param xsdComplexType the XSD complex type
     * @return a unique type name for this complex type
     */
    private static String getComplexTypeName(XmlSchemaComplexType xsdComplexType) {
        return xsdComplexType.getName();
    }

    /**
     * For now a field name is just the XSD element local name. This might
     * change in the future.
     * 
     * @param xsdElement the XSD element
     * @return a field name to use for this element in its parent complex type
     */
    private static String getFieldName(XmlSchemaElement xsdElement) {
        return xsdElement.getName();
    }

    /**
     * A choice is given a name built using its first alternative name (choices
     * do not have names in the XSD).
     * 
     * @param xsdChoice the XSD choice
     * @return a field name to use for this choice in its parent complex type
     */
    private static String getFieldName(XmlSchemaChoice xsdChoice) {
        for (XmlSchemaChoiceMember alternative : xsdChoice.getItems()) {
            if (alternative instanceof XmlSchemaElement) {
                return getFieldName((XmlSchemaElement) alternative)
                        + CHOICE_FIELD_NAME_SUFFIX;
            }

        }
        throw new Xsd2ConverterException(
                "Choice without any alternative at line "
                        + xsdChoice.getLineNumber());
    }

    /**
     * Choices are artificially associated with a complex type name.
     * 
     * @param xsdChoice the XSD choice element
     * @return a unique complex type name for this choice
     */
    private static String getComplexTypeName(XmlSchemaChoice xsdChoice) {
        return StringUtils.capitalize(getFieldName(xsdChoice));
    }

    /**
     * Contribute array-related properties.
     * <p/>
     * If this is a array depending on, there must me a corresponding ODO Object
     * that we enrich with the array dimensions as the numeric range.
     * 
     * @param xsdElement the xsd element marked as an array
     * @param cobolAnnotations the xsd element COBOL annotations
     * @param props the corresponding set of properties
     */
    @SuppressWarnings("unchecked")
    private void addArrayProps(XmlSchemaElement xsdElement,
            CobolAnnotations cobolAnnotations, Map < String, Object > props) {
        props.put("minOccurs", xsdElement.getMinOccurs());
        props.put("maxOccurs", xsdElement.getMaxOccurs());
        String dependingOn = cobolAnnotations.getDependingOn();
        if (dependingOn != null) {
            Map < String, Object > depProps = (Map < String, Object >) odoObjects
                    .get(dependingOn);
            depProps.put("minInclusive",
                    Long.toString(xsdElement.getMinOccurs()));
            depProps.put("maxInclusive",
                    Long.toString(xsdElement.getMaxOccurs()));

            props.put("dependingOn", odoObjectNames.get(dependingOn));
        }
    }

    /**
     * Retrieve the properties of a primitive type.
     * 
     * @param xsdSimpleType the XML schema primitive type
     * @param cobolAnnotations the associated COBOL annotations
     * @return a set of properties
     */
    private Map < String, Object > getProps(XmlSchemaSimpleType xsdSimpleType,
            final CobolAnnotations cobolAnnotations) {
        XmlSchemaSimpleTypeRestriction restriction = (XmlSchemaSimpleTypeRestriction) xsdSimpleType
                .getContent();
        if (restriction != null && restriction.getBaseTypeName() != null) {
            QName xsdTypeName = restriction.getBaseTypeName();
            List < XmlSchemaFacet > facets = restriction.getFacets();
            if (xsdTypeName.equals(Constants.XSD_STRING)) {
                return getCobolAlphanumType(facets);
            } else if (xsdTypeName.equals(Constants.XSD_HEXBIN)) {
                return getCobolOctetStreamType(facets);
            } else if (xsdTypeName.equals(Constants.XSD_INT)) {
                return getCobolDecimalType(cobolAnnotations, Integer.class);
            } else if (xsdTypeName.equals(Constants.XSD_LONG)) {
                return getCobolDecimalType(cobolAnnotations, Long.class);
            } else if (xsdTypeName.equals(Constants.XSD_SHORT)) {
                return getCobolDecimalType(cobolAnnotations, Short.class);
            } else if (xsdTypeName.equals(Constants.XSD_DECIMAL)) {
                return getCobolDecimalType(cobolAnnotations, BigDecimal.class);
            } else if (xsdTypeName.equals(Constants.XSD_FLOAT)) {
                return getCobolDecimalType(cobolAnnotations, Float.class);
            } else if (xsdTypeName.equals(Constants.XSD_DOUBLE)) {
                return getCobolDecimalType(cobolAnnotations, Double.class);
            } else if (xsdTypeName.equals(Constants.XSD_UNSIGNEDINT)) {
                return getCobolDecimalType(cobolAnnotations, Long.class);
            } else if (xsdTypeName.equals(Constants.XSD_UNSIGNEDSHORT)) {
                return getCobolDecimalType(cobolAnnotations, Integer.class);
            } else if (xsdTypeName.equals(Constants.XSD_UNSIGNEDLONG)) {
                return getCobolDecimalType(cobolAnnotations, BigInteger.class);
            } else if (xsdTypeName.equals(Constants.XSD_INTEGER)) {
                return getCobolDecimalType(cobolAnnotations, BigInteger.class);
            } else {
                throw new Xsd2ConverterException("Unsupported xsd type "
                        + xsdTypeName);
            }

        } else {
            throw new Xsd2ConverterException("Simple type without restriction "
                    + xsdSimpleType.getQName());
        }

    }

    /**
     * Retrieve the properties of an alphanumeric type.
     * 
     * @param facets the XSD facets
     * @return the properties of an alphanumeric type
     */
    private <T extends Number> Map < String, Object > getCobolAlphanumType(
            List < XmlSchemaFacet > facets) {
        Map < String, Object > props = new HashMap < String, Object >();
        props.put("cobolTypeName", "CobolStringType");
        props.put("charNum", getMaxLength(facets));
        props.put("javaTypeName", getShortTypeName(String.class));
        return props;
    }

    /**
     * Retrieve the properties of an octet stream type.
     * 
     * @param facets the XSD facets
     * @return the properties of an octet stream type
     */
    private <T extends Number> Map < String, Object > getCobolOctetStreamType(
            List < XmlSchemaFacet > facets) {
        Map < String, Object > props = new HashMap < String, Object >();
        props.put("cobolTypeName", "CobolStringType");
        props.put("charNum", getMaxLength(facets));
        props.put("javaTypeName", getShortTypeName(ByteBuffer.class));
        return props;
    }

    /**
     * Retrieve the maxLength facet if it exists.
     * 
     * @param facets the list of facets
     * @return the maxlength value or -1 if there are no maxLength facets
     */
    private int getMaxLength(List < XmlSchemaFacet > facets) {
        for (XmlSchemaFacet facet : facets) {
            if (facet instanceof XmlSchemaMaxLengthFacet) {
                return Integer
                        .parseInt((String) ((XmlSchemaMaxLengthFacet) facet)
                                .getValue());
            }
        }
        return -1;
    }

    /**
     * Retrieve the properties of a decimal type.
     * 
     * @param cobolAnnotations the current set of COBOL annotations
     * @param clazz the target java numeric type
     * @return the properties of a decimal type
     */
    private <T extends Number> Map < String, Object > getCobolDecimalType(
            CobolAnnotations cobolAnnotations, Class < T > clazz) {
        String cobolType = cobolAnnotations.getCobolType();
        Map < String, Object > props = new HashMap < String, Object >();

        switch (CobolTypes.valueOf(cobolType)) {
        case ZONED_DECIMAL_ITEM:
            props.put("cobolTypeName", "CobolZonedDecimalType");
            props.put("signLeading", cobolAnnotations.signLeading());
            props.put("signSeparate", cobolAnnotations.signSeparate());
            break;
        case PACKED_DECIMAL_ITEM:
            props.put("cobolTypeName", "CobolPackedDecimalType");
            break;
        case BINARY_ITEM:
            props.put("cobolTypeName", "CobolBinaryType");
            break;
        case NATIVE_BINARY_ITEM:
            props.put("cobolTypeName", "CobolBinaryType");
            props.put("minInclusive", "");
            props.put("maxInclusive", "");
            break;
        case SINGLE_FLOAT_ITEM:
            props.put("cobolTypeName", "CobolFloatType");
            break;
        case DOUBLE_FLOAT_ITEM:
            props.put("cobolTypeName", "CobolDoubleType");
            break;
        default:
            throw new Xsd2ConverterException("Unsupported COBOL numeric type "
                    + cobolType);
        }
        props.put("signed", cobolAnnotations.signed());
        props.put("totalDigits", cobolAnnotations.totalDigits());
        props.put("fractionDigits", cobolAnnotations.fractionDigits());
        props.put("javaTypeName", getShortTypeName(clazz));

        if (cobolAnnotations.odoObject()) {
            props.put("odoObject", true);
            odoObjects.put(cobolAnnotations.getCobolName(), props);
        }

        return props;

    }

    /**
     * For java.lang types, strips the package which is not needed in generated
     * java classes.
     * 
     * @param javaType the proposed java class
     * @return the java type name shortened if needed
     */
    private static String getShortTypeName(Class < ? > javaType) {
        String javaTypeName = javaType.getName();
        if (javaTypeName.startsWith("java.lang.")) {
            return javaTypeName.substring("java.lang.".length());
        } else {
            return javaTypeName;
        }
    }

}
