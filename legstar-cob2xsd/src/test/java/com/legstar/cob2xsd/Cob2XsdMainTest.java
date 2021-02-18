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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.nio.charset.StandardCharsets;

import org.apache.commons.cli.Options;
import org.apache.commons.io.FileUtils;
import org.junit.Test;



/**
 * Test the executable jar.
 * 
 */
public class Cob2XsdMainTest extends AbstractTest {

    private static final File tempFolder = new File(
            System.getProperty("java.io.tmpdir"));
    /**
     * Test without arguments.
     */
    @Test
    public void testNoArgument() {
        try {
            Cob2XsdMain main = new Cob2XsdMain();
            Options options = main.createOptions();
            assertTrue(main.collectOptions(options, null));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with help argument.
     */
    @Test
    public void testHelpArgument() {
        try {
            Cob2XsdMain main = new Cob2XsdMain();
            Options options = main.createOptions();
            assertFalse(main.collectOptions(options, new String[] { "-h" }));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with bad input file.
     */
    @Test
    public void testWrongInputArgument() {
        try {
            Cob2XsdMain main = new Cob2XsdMain();
            Options options = main.createOptions();
            main.collectOptions(options, new String[] { "-i nope" });
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.lang.IllegalArgumentException: Input file or folder nope not found",
                    e.toString());
        }
    }

    /**
     * Test with unsupported argument.
     */
    @Test
    public void testUnsupportedArgument() {
        try {
            Cob2XsdMain main = new Cob2XsdMain();
            Options options = main.createOptions();
            main.collectOptions(options, new String[] { "- #" });
        } catch (Exception e) {
            assertEquals(
                    "org.apache.commons.cli.UnrecognizedOptionException: Unrecognized option: - #",
                    e.toString());
        }
    }

    /**
     * Test with configuration argument.
     */
    @Test
    public void testConfigurationArgument() {
        Cob2XsdMain main = new Cob2XsdMain();
        try {
            main.execute(new String[] { "-c",
                    "src/main/resources/cob2xsd.properties", "-i",
                    COBOL_SAMPLES_DIR + "/LSFILEAE", "-o",
                    tempFolder.getAbsolutePath() + "/myfile.xsd",
                    "-n", "http://legstar.com"});
            File result = new File(tempFolder, "myfile.xsd");
            assertTrue(result.exists());
            assertTrue(FileUtils.readFileToString(result, StandardCharsets.UTF_8).contains(
                    "xmlns:tns=\"http://legstar.com/lsfileae\""));
            result.deleteOnExit();
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

}
