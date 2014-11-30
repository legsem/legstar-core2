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

import java.io.File;
import java.io.FileReader;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.custommonkey.xmlunit.DetailedDiff;
import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.junit.Before;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import static org.junit.Assert.*;

/**
 * Generic test methods used when XSD are generated and need to be compared with
 * reference XSDs.
 * 
 */
public class AbstractXsdTester extends AbstractTest {

    /** Used for XML validation. */
    public static final String JAXP_SCHEMA_LANGUAGE =
            "http://java.sun.com/xml/jaxp/properties/schemaLanguage";

    /** Used for XML validation. */
    public static final String W3C_XML_SCHEMA =
            "http://www.w3.org/2001/XMLSchema";

    /** Used for XML validation. */
    public static final String JAXP_SCHEMA_SOURCE =
            "http://java.sun.com/xml/jaxp/properties/schemaSource";

    /** Used for XML validation. */
    public static final String W3C_XML_SCHEMA_SOURCE =
            "http://www.w3.org/2001/XMLSchema.xsd";

    /** DOM document builder factory. */
    protected DocumentBuilderFactory _docFac;

    /** DOM document factory. */
    protected DocumentBuilder _docBuilder;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        super.setUp();
        _docFac = DocumentBuilderFactory.newInstance();
        _docFac.setNamespaceAware(true);
        _docFac.setValidating(false);
        _docFac.setIgnoringElementContentWhitespace(true);
        _docFac.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
        _docFac.setAttribute(JAXP_SCHEMA_SOURCE, W3C_XML_SCHEMA_SOURCE);
        _docBuilder = _docFac.newDocumentBuilder();

    }

    /**
     * Customization of the XmlUnit assertXMLEqual.
     * 
     * @param xsdFileName XML schema file name
     * @param expected expected result
     * @param result actual result
     */
    protected void compare(
            final String xsdFileName,
            final Document expected,
            final Document result) {
        boolean oldIgnore = XMLUnit.getIgnoreWhitespace();
        XMLUnit.setIgnoreWhitespace(true);
        try {
            DetailedDiff d = new DetailedDiff(new Diff(expected, result));
            assertTrue(xsdFileName + ": expected pieces to be identical, "
                    + d.toString(), d.identical());
        } finally {
            XMLUnit.setIgnoreWhitespace(oldIgnore);
        }
    }

    /**
     * Compare result to expected in the case where the XML is serialized as
     * strings (beware that results are pretty printed and therefore will hold
     * CR and/or LF characters).
     * 
     * @param expected the expected XML Schema as a string
     * @param result the result XML schema as a string
     * @throws Exception if comparison fails
     */
    protected void compare(
            final String expected,
            final String result) throws Exception {
        InputSource expectedSrce = new InputSource(new StringReader(expected));
        InputSource resultSrce = new InputSource(new StringReader(result
                .replace("\r", "").replace("\n", "")));
        compare("",
                _docBuilder.parse(expectedSrce),
                _docBuilder.parse(resultSrce));

    }

    /**
     * Pick an XSD from the file system and return it as a DOM.
     * 
     * @param xsdFile the XSD file
     * @return an XML DOM
     * @throws Exception if loaf fails
     */
    public Document getXMLSchemaAsDoc(final File xsdFile) throws Exception {
        FileReader fr = new FileReader(xsdFile);
        try {
            InputSource is = new InputSource();
            is.setCharacterStream(fr);
            return _docBuilder.parse(is);
        } finally {
            fr.close();
        }
    }

}
