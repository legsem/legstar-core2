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
package com.legstar.cob2xsd.antlr;

import static org.junit.Assert.*;

import java.io.StringReader;

import org.junit.Before;
import org.junit.Test;


/**
 * Test the source cleaner class.
 * 
 */
public class CobolFixedFormatSourceCleanerTest extends AbstractCobolTester {

    /** A shared instance of a fixed format cleaner. */
    private CobolFixedFormatSourceCleaner _cleaner;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        _cleaner = new CobolFixedFormatSourceCleaner(getErrorHandler(), 7, 72);
    }

    /**
     * Null input case.
     */
    @Test
    public void testNull() {
        try {
            clean(null);
            fail();
        } catch (RecognizerException e) {
            assertEquals("COBOL source was null", e.getMessage());
        }
    }

    /**
     * Empty input case.
     */
    @Test
    public void testEmpty() {
        try {
            clean("");
            fail();
        } catch (RecognizerException e) {
            assertEquals(
                    "No data descriptions found. Are you sure this is COBOL source?",
                    e.getMessage());
        }
    }

    /**
     * Check that special characters are removed.
     */
    @Test
    public void testCleaningSequenceNumbers() {
        /* 0 1 2 3 4 5 6 7 */
        /* 123456789012345678901234567890123456789012345678901234567890123456789012 */
        assertEquals("", _cleaner.cleanLine(""));

        assertEquals("", _cleaner.cleanLine("123456"));

        CobolFixedFormatSourceCleaner cleaner = new CobolFixedFormatSourceCleaner(
                getErrorHandler(), 1, 66);
        assertEquals("01 A.", cleaner.cleanLine("01 A."));

        assertEquals("      -", _cleaner.cleanLine("123456-"));

        assertEquals(
                "      -                                                              ABC",
                _cleaner.cleanLine("123456-                                                              ABC123456"));
    }

    /**
     * Check that long separators are trimmed.
     * 
     * @throws CleanerException
     */
    @Test
    public void testCleanLongSeparators() throws CleanerException {

        assertEquals("       01 A  PIC  X(5)  VALUE  5." + LS,
                _cleaner.clean(new StringReader("123456 01 A, PIC; X(5), VALUE, 5.")));
    }

    /**
     * Check that long separators are trimmed but not if within literal.
     * 
     * @throws CleanerException
     */
    @Test
    public void testCleanLongSeparatorsInAlphanumericLiteral()
            throws CleanerException {

        assertEquals("       01 A  PIC  X(5)  VALUE  'AB, C'." + LS,
                _cleaner.clean(new StringReader("123456 01 A, PIC; X(5), VALUE, 'AB, C'.")));
    }

    /**
     * Test that DATA DIVISION is properly delineated.
     */
    @Test
    public void testDataDivision() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();
        assertTrue(_cleaner.isDataDivision("", context));
        assertFalse(_cleaner.isDataDivision(" PROCEDURE DIVISION", context));
        assertFalse(_cleaner.isDataDivision("whatever", context));
        context = new CobolFixedFormatSourceCleaner.CleaningContext();
        assertFalse(_cleaner.isDataDivision("       PROCEDURE DIVISION",
                context));
        assertEquals(
                "Found procedure division in [PROCEDURE DIVISION]. Remaining lines ignored.",
                getErrorHandler().getErrorMessages().get(0));
    }

    /**
     * Check that we correctly identify end of statements.
     */
    @Test
    public void testEndStatementDetection() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A PIC 9.9.", " 01 A PIC 9.9.",
                context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01 A PIC X. ", " 01 A PIC X. ",
                context);
        assertTrue(context.isLookingForLevel());
    }

    /**
     * Check cleaning of simple lines.
     */
    @Test
    public void testCleanSimpleLine() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck("", "", context);
        removeExtraneousCharactersAndCheck(" 01.", " 01.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01", " 01", context);
        assertFalse(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" .", " .", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01 A.", " 01 A.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01   A.", " 01   A.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01   A  .", " 01   A  .", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck("blabla", "", context);
        assertEquals("Extraneous characters ignored: blabla", getErrorHandler()
                .getErrorMessages().get(0));
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(
                "       01  FILEA.   COPY DFH0CFIL.", "       01  FILEA. ",
                context);
        assertEquals("Extraneous characters ignored:   COPY DFH0CFIL.",
                getErrorHandler().getErrorMessages().get(1));
        assertTrue(context.isLookingForLevel());
    }

    /**
     * Test cleaning of multi statement line.
     */
    @Test
    public void testMultiStatementLine() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A. 02 B.", " 01 A. 02 B.",
                context);
        removeExtraneousCharactersAndCheck(" 01 A. 02 B.03 C.",
                " 01 A. 02 B.03 C.", context);
        /* Extraneous characters past closed statement should be wiped out */
        removeExtraneousCharactersAndCheck(" 01 A. 02 B. blabl. 03 C.",
                " 01 A. 02 B.        03 C.", context);
        removeExtraneousCharactersAndCheck(" 01. 02 B.", " 01. 02 B.", context);

    }

    /**
     * Test cleaning of multi line stements.
     */
    @Test
    public void testMultiLineStatement() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A", " 01 A", context);
        assertFalse(context.isLookingForLevel());
        /* Whatever is within the statement should remain untouched */
        removeExtraneousCharactersAndCheck("blabla", "blabla", context);
        assertFalse(context.isLookingForLevel());
        /* Extraneous characters past the closing statement should be wiped out */
        removeExtraneousCharactersAndCheck(". blabla", ". ", context);
        assertTrue(context.isLookingForLevel());

    }

    /**
     * Helper method.
     * 
     * @param line line of code
     * @param expected expected cleaneup result
     * @param context cleaning context
     */
    private void removeExtraneousCharactersAndCheck(final String line,
            final String expected,
            final CobolFixedFormatSourceCleaner.CleaningContext context) {
        CobolFixedFormatSourceCleaner cleaner = new CobolFixedFormatSourceCleaner(
                getErrorHandler(), 7, 12);
        assertEquals(expected,
                cleaner.removeExtraneousCharacters(line, context));
    }

    /**
     * Test cleaning on a complete program source.
     */
    @Test
    public void testCompleteCleaning() {

        /* 0 1 2 3 4 5 6 7 */
        /* 123456789012345678901234567890123456789012345678901234567890123456789012 */
        cleanAndCheck(
                "" + "123456 PROCESS XOPTS(APOST)"
                        + LS
                        + "       IDENTIFICATION DIVISION."
                        + LS
                        + "       PROGRAM-ID. LSFILEAE."
                        + LS
                        + "       DATA DIVISION."
                        + LS
                        + "      * OVERVIEW                                                      *"
                        + LS
                        + "COPY 'JOE' REPLACING ==A== BY ==B==."
                        + LS
                        + "123456 01 DFHCOMMAREA.                                                  123456"
                        + LS
                        + "EJECT"
                        + LS
                        + "          05 COM-NUMBER         PIC 9(6)."
                        + LS
                        + "          05 COM-DATE"
                        + LS
                        + ""
                        + LS
                        + "             PIC X(8)."
                        + LS
                        + "          05 COM-AMOUNT         PIC X(8)."
                        + LS
                        + "          05 A value"
                        + LS
                        + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE"
                        + LS
                        + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK"
                        + LS + "      -              \"LLLLLLLLLLMMMMMMMMMM\"."
                        + LS + "       PROCEDURE DIVISION." + LS
                        + "          MOVE" + LS + "             05 TO B." + LS,
                "" + ""
                        + LS
                        + ""
                        + LS
                        + ""
                        + LS
                        + ""
                        + LS
                        + ""
                        + LS
                        + ""
                        + LS
                        + "       01 DFHCOMMAREA."
                        + LS
                        + ""
                        + LS
                        + "          05 COM-NUMBER         PIC 9(6)."
                        + LS
                        + "          05 COM-DATE"
                        + LS
                        + ""
                        + LS
                        + "             PIC X(8)."
                        + LS
                        + "          05 COM-AMOUNT         PIC X(8)."
                        + LS
                        + "          05 A value"
                        + LS
                        + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE"
                        + LS
                        + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK"
                        + LS + "      -              \"LLLLLLLLLLMMMMMMMMMM\"."
                        + LS + "" + LS + "" + LS + "" + LS);
    }

    /**
     * Test effect of cleaning on source code starting at column1.
     */
    @Test
    public void testCodeStartingColumnOne() {
        cleanAndCheck("01   PO-RECORD1." + LS
                + "     05 RECORD-TYPE  PIC 9 VALUE 1.", "" + LS
                + "      5 RECORD-TYPE  PIC 9 VALUE 1." + LS);

    }

    /**
     * Test removal of comments even containing valid data descriptions.
     */
    @Test
    public void testCommentWithValidDataDescription() {
        cleanAndCheck("" + "        01   PO-RECORD1." + LS
                + "      * 01   PO-RECORD2." + LS + "      / 01   PO-RECORD3.",
                "        01   PO-RECORD1." + LS + "" + LS + "" + LS);

    }

    /**
     * Test that identifiers starting with digits are correctly identified.
     */
    @Test
    public void testIdentifierStartsWithDigit() {
        cleanAndCheck("" + "        01  5500-REC-01." + LS
                + "          05 5500-REC-TYPE      PIC X(01)." + LS
                + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS
                        + "          05 5500-REC-TYPE      PIC X(01)." + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

    }

    /**
     * Test that compiler directives are removed.
     */
    @Test
    public void testCompilerDirectives() {
        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         EJECT."
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         EJECT"
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);
        cleanAndCheck("" + "        01  5500-REC-01" + LS + "         EJECT."
                + LS + "         PIC X(06).", "        01  5500-REC-01" + LS
                + "" + LS + "         PIC X(06)." + LS);
        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         SKIP."
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         SKIP1."
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         SKIP2."
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

        cleanAndCheck("" + "        01  5500-REC-01." + LS + "         SKIP3."
                + LS + "          05 5500-PLAN-NUM      PIC X(06).",
                "        01  5500-REC-01." + LS + "" + LS
                        + "          05 5500-PLAN-NUM      PIC X(06)." + LS);

    }

    @Test
    public void testShouldDetectDelimiterBeforeAlphanumLiteral() {
        cleanAndCheck("       01 A. GARBAGE. 02 B VALUE 'Q'.",
                "       01 A.          02 B VALUE 'Q'." + LS);
    }

    @Test
    public void testShouldIgnoreDelimiterWithinLiteral() {
        cleanAndCheck("       10 FILLER  PIC X(56) VALUE 'CONTO N. W '.",
                "       10 FILLER  PIC X(56) VALUE 'CONTO N. W '." + LS);
    }

    @Test
    public void testShouldDetectAlphanumLiteralNotFollowedByDelimiter() {
        cleanAndCheck("       10 FILLER  VALUE 'CONTO N. W ' PIC X(56).",
                "       10 FILLER  VALUE 'CONTO N. W ' PIC X(56)." + LS);
    }

    @Test
    public void testShouldCleanExtraneousBetweenDataItems() {
        cleanAndCheck(
                "       01 WS-ODO."
                        + LS
                        + "          05 FILLER       PIC X(3) VALUE \"ODO\"."
                        + LS
                        + "          05 WS-ODO-A     PIC 9(2)."
                        + LS
                        + LS
                        + "       LINKAGE SECTION."
                        + LS
                        + "       01 DFHCOMMAREA"
                        + LS
                        + "          05 TABLE-SIZE   PIC 9(2)."
                        + LS
                        + "          05 TABLE-ODO OCCURS 1 TO 100 DEPENDING ON TABLE-SIZE"
                        + LS + "                          PIC X(5).",
                "       01 WS-ODO."
                        + LS
                        + "          05 FILLER       PIC X(3) VALUE \"ODO\"."
                        + LS
                        + "          05 WS-ODO-A     PIC 9(2)."
                        + LS
                        + LS
                        + LS
                        + "       01 DFHCOMMAREA"
                        + LS
                        + "          05 TABLE-SIZE   PIC 9(2)."
                        + LS
                        + "          05 TABLE-ODO OCCURS 1 TO 100 DEPENDING ON TABLE-SIZE"
                        + LS + "                          PIC X(5)." + LS);
    }

    /**
     * Issue 60: Long separators are mistakenly replaced in alphanumeric
     * literals.
     */
    @Test
    public void testLongSeparatorsWithinValue() {

        CobolFixedFormatSourceCleaner.CleaningContext context = new CobolFixedFormatSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck("01 A PIC  X(5) VALUE  'AB, C'.",
                "01 A PIC  X(5) VALUE  'AB, C'.", context);
    }

}
