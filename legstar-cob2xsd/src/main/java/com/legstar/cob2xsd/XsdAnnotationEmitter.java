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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.legstar.cobol.model.CobolMarkup;
import com.legstar.cobol.model.CobolTypes;
import com.legstar.cobol.utils.ValueUtil;

/**
 * This class is used to generate LegStar annotations to be included in an XML
 * schema.
 * <p>
 * The COBOL annotations are at the element level and belong to the LegStar
 * namespace.
 */
public class XsdAnnotationEmitter {

    /** This properties file holds the annotation names and namespaces. */
    private static final String ANNOTATIONS_FILE_NAME = "annotations.properties";

    /** The XML Schema being built. */
    private XmlSchema _xsd;

    /** Holds the annotations values. */
    private Properties _annotations = new Properties();;

    /** This builder is used for annotation markup elements. */
    private DocumentBuilder _docBuilder;

    /** True if properly initialized. */
    private boolean _initialized = false;

    /** The translator options in effect. */
    private Cob2XsdConfig _config;

    /** Logger. */
    private static final Logger _log = LoggerFactory
            .getLogger(XsdAnnotationEmitter.class);

    /**
     * All annotations are externalized in a properties file loaded from the
     * classpath. We also create a DOM document builder that is needed to create
     * the custom annotations markup. Errors, which are unlikely, are signaled
     * by logging appropriate messages but no exceptions are raised. Rather, the
     * class disables itself and no annotations are produced.
     * 
     * @param xsd the XML Schema to be populated.
     * @param config the translator options in effect
     */
    public XsdAnnotationEmitter(final XmlSchema xsd, final Cob2XsdConfig config) {

        InputStream is = null;
        try {
            _xsd = xsd;
            _config = config;

            is = XsdAnnotationEmitter.class
                    .getResourceAsStream(ANNOTATIONS_FILE_NAME);
            if (is == null) {
                _log.error("Was unable to locate file " + ANNOTATIONS_FILE_NAME
                        + " from the classpath");
            } else {
                _annotations.load(is);
                DocumentBuilderFactory docFac = DocumentBuilderFactory
                        .newInstance();
                docFac.setNamespaceAware(true);
                _docBuilder = docFac.newDocumentBuilder();
                addNamespaceContext();
                _initialized = true;
            }
        } catch (IOException e) {
            _log.error("Unable to load file " + ANNOTATIONS_FILE_NAME, e);
        } catch (ParserConfigurationException e) {
            _log.error("Unable to get DOM document builder ", e);
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException e) {
                    _log.error("Unable to close input stream ", e);
                }
            }
        }
    }

    /**
     * Create an XML Schema annotation with markup corresponding to the original
     * COBOL data item attributes.
     * 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return an XML schema annotation
     */
    public XmlSchemaAnnotation createLegStarAnnotation(
            final XsdDataItem xsdDataItem) {

        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(getCOXBNamespace(), getCOXBElements());
        Element elc = doc.createElementNS(getCOXBNamespace(), getCOXBElement());

        elc.setAttribute(CobolMarkup.LEVEL_NUMBER,
                Integer.toString(xsdDataItem.getLevelNumber()));
        elc.setAttribute(CobolMarkup.COBOL_NAME, xsdDataItem.getCobolName());
        elc.setAttribute(CobolMarkup.TYPE, xsdDataItem.getCobolType()
                .toString());

        if (xsdDataItem.getCobolType() != CobolTypes.GROUP_ITEM) {
            if (xsdDataItem.getPicture() != null) {
                elc.setAttribute(CobolMarkup.PICTURE, xsdDataItem.getPicture());
            }
            if (xsdDataItem.getUsage() != null) {
                elc.setAttribute(CobolMarkup.USAGE,
                        xsdDataItem.getUsageForCobol());
            }
            if (xsdDataItem.isJustifiedRight()) {
                elc.setAttribute(CobolMarkup.IS_JUSTIFIED_RIGHT, "true");
            }
            if (xsdDataItem.getTotalDigits() > 0) {
                elc.setAttribute(CobolMarkup.IS_SIGNED,
                        (xsdDataItem.isSigned()) ? "true" : "false");
                elc.setAttribute(CobolMarkup.TOTAL_DIGITS,
                        Integer.toString(xsdDataItem.getTotalDigits()));
                if (xsdDataItem.getFractionDigits() > 0) {
                    elc.setAttribute(CobolMarkup.FRACTION_DIGITS,
                            Integer.toString(xsdDataItem.getFractionDigits()));
                }
                if (xsdDataItem.isSignLeading()) {
                    elc.setAttribute(CobolMarkup.IS_SIGN_LEADING, "true");
                }
                if (xsdDataItem.isSignSeparate()) {
                    elc.setAttribute(CobolMarkup.IS_SIGN_SEPARATE, "true");
                }
            }
        }

        /*
         * Annotations transfer the COBOL occurs semantic (as opposed to the XSD
         * semantic). No depending on => fixed size array
         */
        if (xsdDataItem.getCobolMaxOccurs() > 0) {
            elc.setAttribute(CobolMarkup.MAX_OCCURS,
                    Integer.toString(xsdDataItem.getCobolMaxOccurs()));
            if (xsdDataItem.getDependingOn() == null) {
                elc.setAttribute(CobolMarkup.MIN_OCCURS,
                        Integer.toString(xsdDataItem.getCobolMaxOccurs()));
            } else {
                elc.setAttribute(CobolMarkup.DEPENDING_ON,
                        xsdDataItem.getDependingOn());
                elc.setAttribute(CobolMarkup.MIN_OCCURS,
                        Integer.toString(xsdDataItem.getCobolMinOccurs()));
            }
        }

        if (xsdDataItem.isODOObject()) {
            elc.setAttribute(CobolMarkup.IS_ODO_OBJECT, "true");
        }
        if (xsdDataItem.getRedefines() != null) {
            elc.setAttribute(CobolMarkup.REDEFINES, xsdDataItem.getRedefines());
        }
        if (xsdDataItem.isRedefined()) {
            elc.setAttribute(CobolMarkup.IS_REDEFINED, "true");
            elc.setAttribute(CobolMarkup.UNMARSHAL_CHOICE_STRATEGY, "");
        }

        if (xsdDataItem.getValue() != null
                && xsdDataItem.getValue().length() > 0) {
            elc.setAttribute(CobolMarkup.VALUE, ValueUtil.resolveFigurative(
                    xsdDataItem.getValue(), xsdDataItem.getMaxStorageLength(),
                    getConfig().quoteIsQuote()));
        }

        if (xsdDataItem.getSrceLine() > 0) {
            elc.setAttribute(CobolMarkup.SRCE_LINE,
                    Integer.toString(xsdDataItem.getSrceLine()));
        }

        el.appendChild(elc);

        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        appInfo.setMarkup(el.getChildNodes());

        /* Create annotation */
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        annotation.getItems().add(appInfo);
        return annotation;
    }

    /**
     * Adds the COXB namespace and associated prefixes to the XML schema.
     */
    protected void addNamespaceContext() {
        NamespaceMap prefixmap = new NamespaceMap();
        NamespacePrefixList npl = getXsd().getNamespaceContext();
        if (npl == null) {
            /* We get an NPE if we don't add this. */
            prefixmap.add("", XMLConstants.W3C_XML_SCHEMA_NS_URI);
        } else {
            for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
                prefixmap.add(npl.getDeclaredPrefixes()[i],
                        npl.getNamespaceURI(npl.getDeclaredPrefixes()[i]));
            }
        }
        prefixmap.add(getCOXBNamespacePrefix(), getCOXBNamespace());
        getXsd().setNamespaceContext(prefixmap);

    }

    /**
     * @return the COXB namespace
     */
    public String getCOXBNamespace() {
        return _annotations.getProperty("coxb-namespace");
    }

    /**
     * @return the COXB namespace prefix
     */
    public String getCOXBNamespacePrefix() {
        return _annotations.getProperty("coxb-ns-prefix");
    }

    /**
     * @return the COXB qualified elements element
     */
    public String getCOXBElements() {
        return getCOXBNamespacePrefix() + ':'
                + _annotations.getProperty("coxb-elements");
    }

    /**
     * @return the COXB qualified element element
     */
    public String getCOXBElement() {
        return getCOXBNamespacePrefix() + ':'
                + _annotations.getProperty("coxb-element");
    }

    /**
     * @return true if properly initialized
     */
    public boolean initialized() {
        return _initialized;
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
