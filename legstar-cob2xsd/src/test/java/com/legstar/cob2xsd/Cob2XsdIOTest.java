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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

import com.legstar.base.utils.FileUtils;

import static org.junit.Assert.*;

/**
 * Test the COBOL to XSD API.
 * 
 */
public class Cob2XsdIOTest extends AbstractXsdTester {

    /** Logger. */
    private static final Logger _log = LoggerFactory
            .getLogger(Cob2XsdIOTest.class);

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    private static final File tempDir = new File(
            System.getProperty("java.io.tmpdir"));

    /**
     * Go through all the samples and check with backward compatibility.
     * 
     * @throws Exception if test fails
     */
    @Test
    public void testAllSamples() throws Exception {
        File cobolDir = new File(COBOL_SAMPLES_DIR);
        for (File cobolFile : cobolDir.listFiles()) {
            if (cobolFile.isFile()) {
                _log.debug("Translating " + cobolFile);
                String name = cobolFile.getName().toLowerCase();
                configProps.put(Cob2XsdConfig.ADD_LEGSTAR_ANNOTATIONS,
                        Boolean.toString(true));

                /* Backward compatibility */
                configProps.put(
                        Cob2XsdConfig.ELEMENT_NAMES_START_WITH_UPPERCASE,
                        Boolean.toString(true));
                configProps.put(Cob2XsdConfig.QUOTE_IS_QUOTE,
                        Boolean.toString(false));

                Cob2XsdIO translator = new Cob2XsdIO(new Cob2XsdConfig(
                        configProps));
                File custmXslt = new File(XSLT_SAMPLES_DIR, name + ".xsl");
                File xsdFile = translator.translate(cobolFile, "ISO-8859-1",
                        tempDir, "http://legstar.com/test/coxb", custmXslt.exists() ? custmXslt.getPath() : null);
                if (_log.isDebugEnabled()) {
                    _log.debug("Result:\n"
                            + FileUtils.readFileToString(xsdFile));
                }
                File xsdRefFile = new File(XSD_REFERENCES_DIR,
                        name.toLowerCase() + ".xsd");

                if (CREATE_REFERENCES) {
                	Files.write(xsdRefFile.toPath(), Files.readAllBytes(xsdFile.toPath()));
                } else {
                    Document result = getXMLSchemaAsDoc(xsdFile);
                    Document expected = getXMLSchemaAsDoc(xsdRefFile);
                    compare(xsdFile.getName(), expected, result);
                }
                xsdFile.deleteOnExit();
            }
        }
    }

    /**
     * Check that the XML Schema produced has the correct encoding from a file
     * standpoint.
     */
    @Test
    public void testFileOutputEncoding() {
        try {
            configProps.put(Cob2XsdConfig.XSD_ENCODING, "UTF-8");
            configProps.put(Cob2XsdConfig.ADD_LEGSTAR_ANNOTATIONS,
                    Boolean.toString(true));

            Cob2XsdIO cob2xsd = new Cob2XsdIO(new Cob2XsdConfig(configProps));
            File tempCobolFile = File.createTempFile("test", ".cob");
            tempCobolFile.deleteOnExit();

            FileUtils.writeStringToFile(tempCobolFile,
                    "       01 A.\n           02 B PIC G(4) VALUE '牛年快乐'.");
            File xmlSchema = cob2xsd.translate(tempCobolFile, "UTF8", tempDir,
                    "http://www.mycompany.com/test", null);

            for (String line : Files.readAllLines(xmlSchema.toPath(), StandardCharsets.UTF_8)) {
                if (line.contains("cobolName=\"B\"")) {
                    assertTrue(line.contains("value=\"牛年快乐\""));
                }
            }
            xmlSchema.deleteOnExit();

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }

    }

    /**
     * Check input file validation.
     */
    @Test
    public void testInputFileValidation() {
        try {
            Cob2XsdIO cob2xsd = new Cob2XsdIO(defaultConfig);
            cob2xsd.checkCobolSourceFile(null);
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: You must provide a COBOL source file",
                    e.toString());
        }
        try {
            Cob2XsdIO cob2xsd = new Cob2XsdIO(defaultConfig);
            cob2xsd.checkCobolSourceFile(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: COBOL source  file toto not found",
                    e.toString());
        }
    }

    /**
     * Check output file/folder validation.
     */
    @Test
    public void testOutputFileValidation() {
        try {
            Cob2XsdIO cob2xsd = new Cob2XsdIO(defaultConfig);
            cob2xsd.checkTarget(null);
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: You must provide a target directory or file",
                    e.toString());
        }
        try {
            Cob2XsdIO cob2xsd = new Cob2XsdIO(defaultConfig);
            cob2xsd.checkTarget(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: Target folder toto not found",
                    e.toString());
        }
        try {
            Cob2XsdIO cob2xsd = new Cob2XsdIO(defaultConfig);
            cob2xsd.checkTarget(new File("toto.xsd"));
        } catch (Exception e) {
            fail(e.toString());
        }
    }

}
