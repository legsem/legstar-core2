package com.legstar.cob2xsd;

import java.io.File;
import java.io.StringReader;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import static org.junit.Assert.*;

/**
 * These tests check that the XML Schemas that we generate are validating.
 * 
 */
public class Cob2XsdValidationTest extends AbstractXsdTester {

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(Cob2XsdValidationTest.class);

    /** Used as source to XML SChemas generation. */
    private static final String COBOL_SOURCE =
            "*\n"
                    + "01  WS71-HEADER.\n"
                    + "      05  WS71-HEADER-ID        PIC X(4)  VALUE '$HD$'.\n"
                    + "      05  WS73-INVOICE-NO.\n"
                    + "           07  WS73-INVOICE-PREF     PIC X(4).\n";

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp();
    }

    /**
     * Test without a target namespace.
     */
    public void testWithoutNamespace() {
        genSchemaAndParseInstance(null,
                "<ws71Header>"
                        + "<ws71HeaderId>hd12</ws71HeaderId>"
                        + "<ws73InvoiceNo>"
                        + "<ws73InvoicePref>pf12</ws73InvoicePref>"
                        + "</ws73InvoiceNo>"
                        + "</ws71Header>");

    }

    /**
     * Test with a target namespace.
     */
    public void testWithNamespace() {
        genSchemaAndParseInstance("http://legstar.com/test/coxb/",
                "<ws71Header xmlns=\"http://legstar.com/test/coxb/\">"
                        + "<ws71HeaderId>hd12</ws71HeaderId>"
                        + "<ws73InvoiceNo>"
                        + "<ws73InvoicePref>pf12</ws73InvoicePref>"
                        + "</ws73InvoiceNo>"
                        + "</ws71Header>");

    }

    /**
     * Produce an XML schema and validate it. Then parse an XML instance
     * validating against the XML schema.
     * 
     * @param targetNamespace the target namespace or null if none
     * @param xmlInstance an XML instance to validate
     */
    protected void genSchemaAndParseInstance(final String targetNamespace,
            final String xmlInstance) {
        try {
            configProps.put(Cob2XsdConfig.CODE_FORMAT, "FREE_FORMAT");
            configProps.put(Cob2XsdConfig.TARGET_NAMESPACE, targetNamespace);
            Cob2Xsd cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
            String xmlSchema = cob2xsd.translate(new StringReader(COBOL_SOURCE));

            // First make sure the XML Schema itself is valid XML
            // Turned off because W3C site replies with HTTP 500 on
            // parseAndValidate(xmlSchema, W3C_XML_SCHEMA_SOURCE);

            // Store it in a temporary file
            File tempXsdFile = File.createTempFile("test", ".xsd");
            tempXsdFile.deleteOnExit();
            FileUtils.writeStringToFile(tempXsdFile, xmlSchema);

            if (_log.isDebugEnabled()) {
                _log.debug(tempXsdFile.toURI().toString());
                _log.debug(FileUtils.readFileToString(tempXsdFile));
                _log.debug(xmlInstance);
            }

            // Now make sure the XML instance is valid against the XML Schema
            parseAndValidate(xmlInstance, tempXsdFile.toURI().toString());

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Parse an XML instance requesting validation against an Xml schema.
     * 
     * @param xmlInstance the XML instance
     * @param xsdLocation the XML schema location
     */
    protected void parseAndValidate(final String xmlInstance,
            final String xsdLocation) {
        try {
            InputSource xmlSource = new InputSource(new StringReader(
                    xmlInstance));
            _docFac.setValidating(true);
            _docFac.setAttribute(JAXP_SCHEMA_SOURCE, xsdLocation);
            _docBuilder = _docFac.newDocumentBuilder();
            _docBuilder.setErrorHandler(
                    new ErrorHandler() {

                        public void error(final SAXParseException exception)
                                throws SAXException {
                            exception.printStackTrace();
                            fail();

                        }

                        public void fatalError(final SAXParseException exception)
                                throws SAXException {
                            exception.printStackTrace();
                            fail();
                        }

                        public void warning(final SAXParseException exception)
                                throws SAXException {
                            exception.printStackTrace();
                            fail();
                        }

                    });
            _docBuilder.parse(xmlSource);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }

    }

}
