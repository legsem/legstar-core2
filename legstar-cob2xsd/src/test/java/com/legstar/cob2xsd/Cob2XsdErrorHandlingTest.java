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

import org.junit.Test;

import static org.junit.Assert.*;



/**
 * Check how various types of errors are handled at the API level.
 * 
 */
public class Cob2XsdErrorHandlingTest extends AbstractTest {
    
    /**
     * Cleaning might get errors.
     */
    @Test
    public void testCleanerErrors() {
        try {
            translate("^�@");
            fail();
        } catch (XsdGenerationException e) {
            assertEquals(
                    "No data descriptions found. Are you sure this is COBOL source?",
                    e.getCause().getMessage());
        }
    }

    /**
     * Lexing might get errors but they are all recovered from.
     */
    @Test
    public void testLexerErrors() {
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
            cob2xsd.translate(new StringReader("       1 ^�@ ."), "http://test.legstar");
            assertEquals("line 1:12 Syntax error in last COBOL clause", cob2xsd
                    .getErrorHistory().get(0));
        } catch (XsdGenerationException e) {
            fail();
        }
    }

    /**
     * Parsing might get UnwantedTokenException.
     */
    @Test
    public void testParserUnwantedTokenException() {
        try {
            translate("       01 01.");
            fail();
        } catch (XsdGenerationException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:10 unexpected token '01' expecting PERIOD",
                    e.getCause().getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException.
     */
    @Test
    public void testParserMismatchedTokenException() {
        try {
            translate("       88 A PIC X.");
            fail();
        } catch (XsdGenerationException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:12 unexpected token 'PIC' expecting VALUE_KEYWORD",
                    e.getCause().getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException and reaches end of file.
     */
    @Test
    public void testParserMismatchedTokenExceptionEOF() {
        try {
            translate("       01 A PIC X");
            fail();
        } catch (XsdGenerationException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 2:0 reached end of file looking for PERIOD",
                    e.getCause().getMessage());
        }
    }

    /**
     * Parsing might get EarlyExitException.
     */
    @Test
    public void testParserEarlyExitException() {
        try {
            translate("       01 A PIC.");
            fail();
        } catch (XsdGenerationException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:15 required tokens not found at input '.'",
                    e.getCause().getMessage());
        }
    }

}
