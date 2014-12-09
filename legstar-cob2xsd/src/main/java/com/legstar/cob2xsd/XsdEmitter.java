/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaChoice;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaEnumerationFacet;
import org.apache.ws.commons.schema.XmlSchemaFractionDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaMaxInclusiveFacet;
import org.apache.ws.commons.schema.XmlSchemaMaxLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaMinInclusiveFacet;
import org.apache.ws.commons.schema.XmlSchemaPatternFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.XmlSchemaTotalDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cob2xsd.XsdDataItem.XsdType;
import com.legstar.cobol.model.CobolDataItem.DataEntryType;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.cobol.model.CobolTypes;
import com.legstar.cobol.utils.ValueUtil;

/**
 * Populates an XML Schema from COBOL data items (COBOL Model).
 * <p/>
 * Uses the {@link XsdDataItem} facade to CobolDataItem. This will have all XSD
 * attributes ready.
 * <p/>
 * This class uses Apache XmlSchema to produce the XML schema.
 * <p/>
 * All execution parameters are bundled in {@link Cob2XsdConfig}.
 * 
 */
public class XsdEmitter {

    /** The XML Schema being built. */
    private XmlSchema _xsd;

    /** The translator options in effect. */
    private Cob2XsdConfig _config;

    /** Specialized LegStar/JAXB annotations emitter. */
    private XsdAnnotationEmitter _annotationEmitter;

    /** Logger. */
    private static final Logger _log = LoggerFactory
            .getLogger(XsdEmitter.class);

    /** TODO make XSD name formatting optional */

    /**
     * Constructor.
     * 
     * @param xsd the XML Schema to be populated.
     * @param model the translator options
     */
    public XsdEmitter(final XmlSchema xsd, final Cob2XsdConfig model) {
        _xsd = xsd;
        _config = model;
        if (_config.addLegStarAnnotations()) {
            _annotationEmitter = new XsdAnnotationEmitter(xsd, model);
        }
    }

    /**
     * Maps a COBOL data item to an XML schema type.
     * <ul>
     * <li>COBOL elementary data items are mapped to XML Schema simple types.</li>
     * <li>COBOL structures are mapped to XML schema complex Types.</li>
     * </ul>
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return a corresponding XML schema type
     */
    public XmlSchemaType createXmlSchemaType(final XsdDataItem xsdDataItem) {
        if (xsdDataItem.getXsdType() == null) {
            return null;
        }
        switch (xsdDataItem.getXsdType()) {
        case COMPLEX:
            return createXmlSchemaComplexType(xsdDataItem);
        case STRING:
            return createAlphaXmlSchemaSimpleType(xsdDataItem, "string");
        case HEXBINARY:
            return createAlphaXmlSchemaSimpleType(xsdDataItem, "hexBinary");
        case SHORT:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "short");
        case USHORT:
            return createNumericXmlSchemaSimpleType(xsdDataItem,
                    "unsignedShort");
        case INT:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "int");
        case UINT:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "unsignedInt");
        case LONG:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "long");
        case ULONG:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "unsignedLong");
        case INTEGER:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "integer");
        case DECIMAL:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "decimal");
        case FLOAT:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "float");
        case DOUBLE:
            return createNumericXmlSchemaSimpleType(xsdDataItem, "double");
        default:
            return null;
        }
    }

    /**
     * Create an XML schema complex type. We want to use Named complex types so
     * we add them to the XSD directly. We add complex types before their
     * children because its nicer for the XSD layout to list roots before leafs.
     * Redefined and redefining elements are grouped into an XML Schema choice.
     * A choice is created when an element marked as isRedefined is encountered
     * and it groups all subsequent elements marked as redefines until a non
     * redefining element is found.
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return a new complex type
     */
    public XmlSchemaComplexType createXmlSchemaComplexType(
            final XsdDataItem xsdDataItem) {
        
        // All complex types are root complex types
        XmlSchemaComplexType xmlSchemaComplexType = new XmlSchemaComplexType(
                getXsd(), true);

        XmlSchemaChoice xmlSchemaChoice = null;

        XmlSchemaSequence xmlSchemaSequence = new XmlSchemaSequence();
        for (XsdDataItem child : xsdDataItem.getChildren()) {
            XmlSchemaElement xmlSchemaElement = createXmlSchemaElement(child);
            if (xmlSchemaElement != null) {
                if (xmlSchemaChoice == null) {
                    if (child.isRedefined()) {
                        xmlSchemaChoice = new XmlSchemaChoice();
                        xmlSchemaChoice.getItems().add(xmlSchemaElement);
                    } else {
                        xmlSchemaSequence.getItems().add(xmlSchemaElement);
                    }
                } else {
                    if (child.getRedefines() != null) {
                        xmlSchemaChoice.getItems().add(xmlSchemaElement);
                    } else {
                        xmlSchemaSequence.getItems().add(xmlSchemaChoice);
                        xmlSchemaChoice = null;
                        xmlSchemaSequence.getItems().add(xmlSchemaElement);
                    }
                }
            }
        }

        if (xmlSchemaChoice != null) {
            xmlSchemaSequence.getItems().add(xmlSchemaChoice);
            xmlSchemaChoice = null;
        }

        xmlSchemaComplexType.setParticle(xmlSchemaSequence);
        xmlSchemaComplexType.setName(xsdDataItem.getXsdTypeName());

        return xmlSchemaComplexType;
    }

    /**
     * Create an XML Schema element from a COBOL data item.
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return the XML schema element
     */
    public XmlSchemaElement createXmlSchemaElement(final XsdDataItem xsdDataItem) {
        
        // Let call add root elements if he needs to so for now pretend this is not a root element
        XmlSchemaElement element = new XmlSchemaElement(getXsd(),
                false);
        element.setName(xsdDataItem.getXsdElementName());
        if (xsdDataItem.getMaxOccurs() != 1) {
            element.setMaxOccurs(xsdDataItem.getMaxOccurs());
        }
        if (xsdDataItem.getMinOccurs() != 1) {
            element.setMinOccurs(xsdDataItem.getMinOccurs());
        }

        /*
         * Create this element schema type, then if its a simple type set it as
         * an anonymous type. Otherwise, it is a named complex type, so
         * reference it by name.
         */
        XmlSchemaType xmlSchemaType = createXmlSchemaType(xsdDataItem);
        if (xmlSchemaType == null) {
            return null;
        }
        if (xmlSchemaType instanceof XmlSchemaSimpleType) {
            element.setSchemaType(xmlSchemaType);
        } else {
            element.setSchemaTypeName(xmlSchemaType.getQName());
        }
        if (getConfig().addLegStarAnnotations()) {
            element.setAnnotation(_annotationEmitter
                    .createLegStarAnnotation(xsdDataItem));
        }
        return element;
    }

    /**
     * Create a simple type for an alphanumeric type.
     * <p/>
     * COBOL alphanumeric fields are fixed length so we create a facet to
     * enforce that constraint. A pattern derived from the picture clause can
     * also be used as a facet. If the item has children conditions, we add
     * enumeration facets
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @param xsdTypeName the XML schema built-in type name to use as a
     *            restriction
     * @return an XML schema simple type
     */
    protected XmlSchemaSimpleType createAlphaXmlSchemaSimpleType(
            final XsdDataItem xsdDataItem, final String xsdTypeName) {

        XmlSchemaSimpleTypeRestriction restriction = createRestriction(xsdTypeName);
        if (xsdDataItem.getLength() > -1) {
            restriction.getFacets().add(
                    createMaxLengthFacet(xsdDataItem.getLength()));
        }
        if (xsdDataItem.getPattern() != null) {
            restriction.getFacets().add(
                    createPatternFacet(xsdDataItem.getPattern()));
        }
        addEnumerationFacets(xsdDataItem, restriction);
        return createXmlSchemaSimpleType(restriction);
    }

    /**
     * Create a simple type for an numeric type.
     * <p/>
     * Numeric elements might have totaDigits, fractionDigits, minInclusive or
     * maxInclusive facets.
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @param xsdTypeName the XML schema built-in type name to use as a
     *            restriction
     * @return an XML schema simple type
     */
    protected XmlSchemaSimpleType createNumericXmlSchemaSimpleType(
            final XsdDataItem xsdDataItem, final String xsdTypeName) {

        XmlSchemaSimpleTypeRestriction restriction = createRestriction(xsdTypeName);

        /*
         * COBOL native binary are special because even though they have a
         * totalDigits attribute, it is not used enforce a restriction.
         */
        if (xsdDataItem.getCobolType() != CobolTypes.NATIVE_BINARY_ITEM
                && xsdDataItem.getTotalDigits() > -1) {

            /*
             * Due to a bug in JAXB (see JAXB issue 715), unsignedLong may end
             * up being mapped to BigInteger instead of Long when totalDigits is
             * used instead of maxInclusive. So for now, we keep maxInclusive.
             */
            if (xsdDataItem.getXsdType() == XsdType.ULONG) {
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < xsdDataItem.getTotalDigits(); i++) {
                    sb.append("9");
                }
                restriction.getFacets().add(
                        createMaxInclusiveFacet(sb.toString()));
            } else {
                restriction.getFacets().add(
                        createTotalDigitsFacet(xsdDataItem.getTotalDigits()));
            }
        }

        /* fractionDigits is a fixed facet for most numerics so be careful */
        if (xsdDataItem.getFractionDigits() > 0) {
            restriction.getFacets().add(
                    createFractionDigitsFacet(xsdDataItem.getFractionDigits()));
        }

        /*
         * For xsd:decimal and xsd:integer, we further constrain if the numeric
         * needs to be positive (unsigned).
         */
        if ((xsdDataItem.getXsdType() == XsdType.INTEGER || xsdDataItem
                .getXsdType() == XsdType.DECIMAL) && !xsdDataItem.isSigned()) {
            restriction.getFacets().add(createMinInclusiveFacet("0"));
        }
        addEnumerationFacets(xsdDataItem, restriction);
        return createXmlSchemaSimpleType(restriction);
    }

    /**
     * If simple type has conditions attached to it, emit enumeration facets.
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @param restriction the current set of constraints
     */
    protected void addEnumerationFacets(final XsdDataItem xsdDataItem,
            final XmlSchemaSimpleTypeRestriction restriction) {
        if (getConfig().mapConditionsToFacets()) {
            boolean hasValueThru = false;
            for (XsdDataItem child : xsdDataItem.getChildren()) {
                if (child.getDataEntryType() == DataEntryType.CONDITION) {
                    for (String conditionValue : child.getConditionLiterals()) {
                        restriction.getFacets().add(
                                createEnumerationFacet(ValueUtil
                                        .resolveFigurative(conditionValue,
                                                xsdDataItem
                                                        .getMaxStorageLength(),
                                                getConfig().quoteIsQuote())));
                    }
                    for (Range conditionRange : child.getConditionRanges()) {
                        if (hasValueThru) {
                            _log.warn(xsdDataItem.getCobolName()
                                    + " has several VALUE THRU statements."
                                    + " Cannot translate to XSD."
                                    + " Only the first one will be converted."
                                    + " Ignoring: " + conditionRange.toString());
                            break;
                        }
                        restriction.getFacets().add(
                                createMinInclusiveFacet(ValueUtil
                                        .resolveFigurative(conditionRange
                                                .getFrom(), xsdDataItem
                                                .getMaxStorageLength(),
                                                getConfig().quoteIsQuote())));
                        restriction.getFacets().add(
                                createMaxInclusiveFacet(ValueUtil
                                        .resolveFigurative(conditionRange
                                                .getTo(), xsdDataItem
                                                .getMaxStorageLength(),
                                                getConfig().quoteIsQuote())));
                        hasValueThru = true;
                    }

                }
            }
        }
    }

    /**
     * Create an XML schema simple type from a restriction.
     * 
     * @param restriction the XML schema restriction
     * @return the XML schema simple type
     */
    protected XmlSchemaSimpleType createXmlSchemaSimpleType(
            final XmlSchemaSimpleTypeRestriction restriction) {
        XmlSchemaSimpleType xmlSchemaSimpleType = new XmlSchemaSimpleType(
                getXsd(), false);
        xmlSchemaSimpleType.setContent(restriction);
        return xmlSchemaSimpleType;
    }

    /**
     * Create an XML schema restriction.
     * 
     * @param xsdTypeName the XML schema built-in type name to use as a
     *            restriction
     * @return an XML schema restriction
     */
    protected XmlSchemaSimpleTypeRestriction createRestriction(
            final String xsdTypeName) {
        XmlSchemaSimpleTypeRestriction restriction = new XmlSchemaSimpleTypeRestriction();
        restriction.setBaseTypeName(new QName(
                XMLConstants.W3C_XML_SCHEMA_NS_URI, xsdTypeName));
        return restriction;
    }

    /**
     * Create an XML schema maxLength facet.
     * 
     * @param length the value to set
     * @return an XML schema length facet
     */
    protected XmlSchemaMaxLengthFacet createMaxLengthFacet(final int length) {
        XmlSchemaMaxLengthFacet xmlSchemaMaxLengthFacet = new XmlSchemaMaxLengthFacet();
        xmlSchemaMaxLengthFacet.setValue(length);
        return xmlSchemaMaxLengthFacet;
    }

    /**
     * Create an XML schema pattern facet.
     * 
     * @param pattern the value to set
     * @return an XML schema pattern facet
     */
    protected XmlSchemaPatternFacet createPatternFacet(final String pattern) {
        XmlSchemaPatternFacet xmlSchemaPatternFacet = new XmlSchemaPatternFacet();
        xmlSchemaPatternFacet.setValue(pattern);
        return xmlSchemaPatternFacet;
    }

    /**
     * Create an XML schema enumeration facet.
     * 
     * @param conditionValue the value to set
     * @return an XML schema enumeration facet
     */
    protected XmlSchemaEnumerationFacet createEnumerationFacet(
            final String conditionValue) {
        XmlSchemaEnumerationFacet xmlSchemaEnumerationFacet = new XmlSchemaEnumerationFacet();
        xmlSchemaEnumerationFacet.setValue(conditionValue);
        return xmlSchemaEnumerationFacet;
    }

    /**
     * Create an XML schema totalDigits facet.
     * 
     * @param totalDigits the value to set
     * @return an XML schema totalDigits facet
     */
    protected XmlSchemaTotalDigitsFacet createTotalDigitsFacet(
            final int totalDigits) {
        XmlSchemaTotalDigitsFacet xmlSchemaTotalDigitsFacet = new XmlSchemaTotalDigitsFacet();
        xmlSchemaTotalDigitsFacet.setValue(totalDigits);
        return xmlSchemaTotalDigitsFacet;
    }

    /**
     * Create an XML schema fractionDigits facet.
     * 
     * @param fractionDigits the value to set
     * @return an XML schema fractionDigits facet
     */
    protected XmlSchemaFractionDigitsFacet createFractionDigitsFacet(
            final int fractionDigits) {
        XmlSchemaFractionDigitsFacet xmlSchemaFractionDigitsFacet = new XmlSchemaFractionDigitsFacet();
        xmlSchemaFractionDigitsFacet.setValue(fractionDigits);
        return xmlSchemaFractionDigitsFacet;
    }

    /**
     * Create an XML schema minInclusive facet.
     * 
     * @param minInclusive the value to set
     * @return an XML schema minInclusive facet
     */
    protected XmlSchemaMinInclusiveFacet createMinInclusiveFacet(
            final String minInclusive) {
        XmlSchemaMinInclusiveFacet xmlSchemaMinInclusiveFacet = new XmlSchemaMinInclusiveFacet();
        xmlSchemaMinInclusiveFacet.setValue(minInclusive);
        return xmlSchemaMinInclusiveFacet;
    }

    /**
     * Create an XML schema maxInclusive facet.
     * 
     * @param maxInclusive the value to set
     * @return an XML schema maxInclusive facet
     */
    protected XmlSchemaMaxInclusiveFacet createMaxInclusiveFacet(
            final String maxInclusive) {
        XmlSchemaMaxInclusiveFacet xmlSchemaMaxInclusiveFacet = new XmlSchemaMaxInclusiveFacet();
        xmlSchemaMaxInclusiveFacet.setValue(maxInclusive);
        return xmlSchemaMaxInclusiveFacet;
    }

    /**
     * @return the XML Schema being built
     */
    public XmlSchema getXsd() {
        return _xsd;
    }

    /**
     * @return the translator options in effect
     */
    public Cob2XsdConfig getConfig() {
        return _config;
    }

}
