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

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaForm;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import com.legstar.cob2xsd.antlr.RecognizerErrorHandler;
import com.legstar.cobol.model.CobolDataItem;

import static org.custommonkey.xmlunit.XMLAssert.*;

/**
 * General purpose testing helpers.
 * 
 */
public class AbstractXsdEmitterTester extends AbstractTest {

    /** DOM document factory. */
    private DocumentBuilder _docBuilder;

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /** Logger. */
    private static final Logger _log = LoggerFactory
            .getLogger(AbstractXsdEmitterTester.class);

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        super.setUp();
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _docBuilder = docFac.newDocumentBuilder();
    }

    /**
     * Helper that emits XML schema from a COBOL data item and checks result.
     * 
     * @param expected the expected XML Schema (without the schema element for
     *            simplicity)
     * @param dataItem the COBOL data item
     */
    public void emitAndCheck(final String expected, final CobolDataItem dataItem) {
        emitAndCheck(expected, dataItem, false, false);
    }

    /**
     * Helper that emits XML schema from a COBOL data item and checks result.
     * 
     * @param expected the expected XML Schema (without the schema element for
     *            simplicity)
     * @param dataItem the COBOL data item
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     */
    public void emitAndCheck(final String expected,
            final CobolDataItem dataItem, final boolean withLegStarAnnotations) {
        emitAndCheck(expected, dataItem, withLegStarAnnotations, false);
    }

    /**
     * Helper that emits XML schema from a COBOL data item and checks result.
     * 
     * @param expected the expected XML Schema (without the schema element for
     *            simplicity)
     * @param dataItem the COBOL data item
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     * @param mapConditionsToFacets true if facets must be generated for
     *            conditions
     */
    public void emitAndCheck(final String expected,
            final CobolDataItem dataItem, final boolean withLegStarAnnotations,
            final boolean mapConditionsToFacets) {

        configProps.put(Cob2XsdConfig.ADD_LEGSTAR_ANNOTATIONS, Boolean.toString(withLegStarAnnotations));
        configProps.put(Cob2XsdConfig.MAP_CONDITIONS_TO_FACETS,
                Boolean.toString(mapConditionsToFacets));
        Cob2XsdConfig config = new Cob2XsdConfig(configProps);

        XmlSchema xsd = getXmlSchema();
        XsdEmitter emitter = new XsdEmitter(xsd, config);
        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, config, null, 0,
                new ArrayList < String >(), _errorHandler);
        emitter.createXmlSchemaType(xsdDataItem);
        check(expected, xsd, withLegStarAnnotations);
    }

    /**
     * @return an empty XML schema for testing
     */
    public XmlSchema getXmlSchema() {
        XmlSchema xsd = new XmlSchema("http://legstar.com/test",
                new XmlSchemaCollection());
        xsd.setElementFormDefault(XmlSchemaForm.QUALIFIED);
        return xsd;
    }

    /**
     * Helper that checks a result XML schema against an expected one.
     * 
     * @param expected the expected XML Schema (without the schema element for
     *            simplicity)
     * @param xsd the XML schema result
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     */
    public void check(final String expected, final XmlSchema xsd,
            final boolean withLegStarAnnotations) {
        if (_log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            xsd.write(writer);
            _log.debug("result:\n" + writer.toString());
        }
        try {
            assertXMLEqual(
                    getExpectedXMLSchema(expected, withLegStarAnnotations),
                    xsd.getSchemaDocument());
        } catch (Exception e) {
            _log.error("XSD check failed", e);
            org.junit.Assert.fail();
        }
    }

    /**
     * Wrap the expected content in a complete XML schema and make it a DOM.
     * 
     * @param expected the XML Schema content
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     * @return a DOM document
     * @throws Exception if something goes wrong
     */
    public Document getExpectedXMLSchema(final String expected,
            final boolean withLegStarAnnotations) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:tns=\"http://legstar.com/test\""
                + " attributeFormDefault=\"unqualified\""
                + " elementFormDefault=\"qualified\""
                + " targetNamespace=\"http://legstar.com/test\"");
        if (withLegStarAnnotations) {
            sb.append(" xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\"");
        }
        sb.append(">");
        sb.append(expected);
        sb.append("</schema>");
        InputSource is = new InputSource();
        is.setCharacterStream(new StringReader(sb.toString()));
        return _docBuilder.parse(is);
    }

}
