package com.legstar.cobol.model;

import org.apache.commons.lang3.StringUtils;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Encapsulates the XML schema COBOL annotations.
 *
 */
public class CobolAnnotations {

    private final Element cobolAnnotations;

    public CobolAnnotations(XmlSchemaElement xsdElement) {
        this.cobolAnnotations = getCobolXsdAnnotations(xsdElement);
    }

    /**
     * XSD elements are annotated with COBOL markup that we extract here.
     * 
     * @param xsdElement the XSD element
     * @return the COBOL markup
     */
    private static Element getCobolXsdAnnotations(XmlSchemaElement xsdElement) {
        XmlSchemaAnnotation annotation = xsdElement.getAnnotation();
        if (annotation == null || annotation.getItems().size() == 0) {
            throw new IllegalArgumentException("Xsd element of type "
                    + xsdElement.getSchemaType().getQName()
                    + " at line " + xsdElement.getLineNumber()
                    + " does not have COBOL annotations");
        }

        XmlSchemaAppInfo appinfo = (XmlSchemaAppInfo) annotation.getItems()
                .get(0);
        if (appinfo.getMarkup() == null) {
            throw new IllegalArgumentException("Xsd element of type "
                    + xsdElement.getSchemaType().getQName()
                    + " does not have any markup in its annotations");
        }
        Node node = null;
        boolean found = false;
        for (int i = 0; i < appinfo.getMarkup().getLength(); i++) {
            node = appinfo.getMarkup().item(i);
            if (node instanceof Element
                    && node.getLocalName().equals(CobolMarkup.ELEMENT)
                    && node.getNamespaceURI().equals(CobolMarkup.NS)) {
                found = true;
                break;
            }
        }
        if (!found) {
            throw new IllegalArgumentException("Xsd element of type "
                    + xsdElement.getSchemaType().getQName()
                    + " at line " + xsdElement.getLineNumber()
                    + " does not have any COBOL annotations");
        }
        return (Element) node;
    }

   public String getCobolType() {
        return cobolAnnotations.getAttribute(CobolMarkup.TYPE);
    }

    public String getCobolName() {
        return cobolAnnotations.getAttribute(CobolMarkup.COBOL_NAME);
    }

    public boolean signed() {
        String val = cobolAnnotations.getAttribute(CobolMarkup.IS_SIGNED);
        if (StringUtils.isBlank(val)) {
            return false;
        }
        return Boolean.parseBoolean(val);
    }

    public boolean signLeading() {
        String val = cobolAnnotations
                .getAttribute(CobolMarkup.IS_SIGN_LEADING);
        if (StringUtils.isBlank(val)) {
            return false;
        }
        return Boolean.parseBoolean(val);
    }

    public boolean signSeparate() {
        String val = cobolAnnotations
                .getAttribute(CobolMarkup.IS_SIGN_SEPARATE);
        if (StringUtils.isBlank(val)) {
            return false;
        }
        return Boolean.parseBoolean(val);
    }

    public int totalDigits() {
        String val = cobolAnnotations
                .getAttribute(CobolMarkup.TOTAL_DIGITS);
        if (StringUtils.isBlank(val)) {
            return 0;
        }
        return Integer.parseInt(val);

    }

    public int fractionDigits() {
        String val = cobolAnnotations
                .getAttribute(CobolMarkup.FRACTION_DIGITS);
        if (StringUtils.isBlank(val)) {
            return 0;
        }
        return Integer.parseInt(val);

    }

    public boolean odoObject() {
        String val = cobolAnnotations
                .getAttribute(CobolMarkup.IS_ODO_OBJECT);
        if (StringUtils.isBlank(val)) {
            return false;
        }
        return Boolean.parseBoolean(val);
    }

    public String getDependingOn() {
        if (cobolAnnotations.hasAttribute(CobolMarkup.DEPENDING_ON)) {
            return cobolAnnotations.getAttribute(CobolMarkup.DEPENDING_ON);
        } else {
            return null;
        }
    }

}

