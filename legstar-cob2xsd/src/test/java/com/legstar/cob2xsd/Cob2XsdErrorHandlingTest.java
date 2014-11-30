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

import org.junit.Before;
import org.junit.Test;

import com.legstar.cob2xsd.antlr.RecognizerException;

import static org.junit.Assert.*;



/**
 * Check how various types of errors are handled at the API level.
 * 
 */
public class Cob2XsdErrorHandlingTest extends AbstractTest {
    
    private Cob2Xsd cob2xsd;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
    }

    /**
     * Cleaning might get errors.
     */
    @Test
    public void testCleanerErrors() {
        try {
            cob2xsd.translate("^�@");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "No data descriptions found. Are you sure this is COBOL source?",
                    e.getMessage());
        }
    }

    /**
     * Lexing might get errors but they are all recovered from.
     */
    @Test
    public void testLexerErrors() {
        try {
            cob2xsd.translate("       1 ^�@ .");
            assertEquals("line 1:12 Syntax error in last COBOL clause", cob2xsd
                    .getErrorHistory().get(0));
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            fail();
        }
    }

    /**
     * Parsing might get UnwantedTokenException.
     */
    @Test
    public void testParserUnwantedTokenException() {
        try {
            cob2xsd.translate("       01 01.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:10 unexpected token '01' expecting PERIOD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException.
     */
    @Test
    public void testParserMismatchedTokenException() {
        try {
            cob2xsd.translate("       88 A PIC X.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:12 unexpected token 'PIC' expecting VALUE_KEYWORD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException and reaches end of file.
     */
    @Test
    public void testParserMismatchedTokenExceptionEOF() {
        try {
            cob2xsd.translate("       01 A PIC X");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 2:0 reached end of file looking for PERIOD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get EarlyExitException.
     */
    @Test
    public void testParserEarlyExitException() {
        try {
            cob2xsd.translate("       01 A PIC.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "Parsing failed. 1 syntax errors."
                            + " Last error was line 1:15 required tokens not found at input '.'",
                    e.getMessage());
        }
    }

}
