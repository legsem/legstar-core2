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

import org.junit.Test;


/**
 * Test cases for the cobol Lexer.
 * 
 */
public class CobolStructureLexerTest extends AbstractCobolTester {

    /**
     * Comments should be skipped from the token stream. Starting white spaces
     * are not part of the comment and are on the hidden channel
     */
    @Test
    public void testComments() {
        lexAndCheck("" + "       01 A" + LS + "      * a comment" + LS
                + "       ." + LS, "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                + "[@1,10:10='A',<DATA_NAME>,1:10]"
                + "[@2,20:20='.',<PERIOD>,3:7]");
        lexAndCheck("" + "       01 A" + LS + "      / a comment" + LS
                + "       ." + LS, "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                + "[@1,10:10='A',<DATA_NAME>,1:10]"
                + "[@2,20:20='.',<PERIOD>,3:7]");
    }

    /**
     * Items don't have to be named.
     */
    @Test
    public void testLevelAlone() {
        lexAndCheck("       01." + LS, "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                + "[@1,9:9='.',<PERIOD>,1:9]");
    }

    /**
     * An empty string literal.
     */
    @Test
    public void testEmptyLiteralString() {
        lexAndCheck("       1 A value \"\"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:18='\"\"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * A string literal on a single line.
     */
    @Test
    public void testSingleLineLiteralString() {
        lexAndCheck(
                "       1 A value \" a literal \"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:29='\" a literal \"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * A string literal on a single line but with a delimiter that occurs in the
     * middle of the literal (and therefore should be interpreted as part of the
     * literal).
     */
    @Test
    public void testSingleLineLiteralStringWithInnerDelimiter() {
        lexAndCheck(
                "       1 A value \" a li\"\"teral \"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:31='\" a li\"\"teral \"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * Two consecutive strings separated by at least a space should not be
     * confused with a single string.
     */
    @Test
    public void testSingleLineDoubleString() {
        lexAndCheck(
                "       1 A value \" a li\" \"teral \"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:23='\" a li\"',<ALPHANUM_LITERAL_STRING>,1:17]"
                        + "[@4,25:32='\"teral \"',<ALPHANUM_LITERAL_STRING>,1:25]");
    }

    /**
     * A multiline string literal. It should be reassembled into a single
     * literal.
     */
    @Test
    public void testMultilineLiteralString() {
        lexAndCheck(
                "       1 A value"
                        + LS
                        + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE"
                        + LS
                        + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK"
                        + LS + "      -              \"LLLLLLLLLLMMMMMMMMMM\"."
                        + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,38:205='\"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEEGGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKKLLLLLLLLLLMMMMMMMMMM\"',<ALPHANUM_LITERAL_STRING>,2:21]"
                        + "[@4,206:206='.',<PERIOD>,4:43]");
    }

    /**
     * Same as above but with apostrophe delimiters rather than quotes.
     */
    @Test
    public void testMultilineLiteralStringApost() {
        lexAndCheck(
                "       1 A value"
                        + LS
                        + "                     \'AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE"
                        + LS + "      -              \'LLLLLLLLLLMMMMMMMMMM\'.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,38:132=''AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEELLLLLLLLLLMMMMMMMMMM'',<ALPHANUM_LITERAL_STRING>,2:21]"
                        + "[@4,133:133='.',<PERIOD>,3:43]");
    }

    /**
     * An integer value. Should not be extracted with the period delimiter.
     */
    @Test
    public void testIntegerLiteral() {
        lexAndCheck("       1 A value 99.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:18='99',<INT>,1:17]"
                        + "[@4,19:19='.',<PERIOD>,1:19]");
    }

    /**
     * A value with extra IS keyword.
     */
    @Test
    public void testIntegerLiteralWithIs() {
        lexAndCheck("       1 A value is 99.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,20:21='99',<INT>,1:20]"
                        + "[@4,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * An integer value. This time it should be detected as an INT.
     */
    @Test
    public void testIntegerLiteralSeparateFromPeriod() {
        lexAndCheck("       1 A value 99 .",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:18='99',<INT>,1:17]"
                        + "[@4,20:20='.',<PERIOD>,1:20]");
    }

    /**
     * A decimal value. Might confuse lexers because period is a decimal point.
     */
    @Test
    public void testDecimalLiteral() {
        lexAndCheck("       1 A value 99.9.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:18='99',<INT>,1:17]"
                        + "[@4,19:19='.',<DECIMAL_POINT>,1:19]"
                        + "[@5,20:20='9',<INT>,1:20]"
                        + "[@6,21:21='.',<PERIOD>,1:21]");
    }

    /**
     * Same as above but without an integer part.
     */
    @Test
    public void testNoIntegerPartDecimalLiteral() {
        lexAndCheck("       1 A value .99.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:17='.',<DECIMAL_POINT>,1:17]"
                        + "[@4,18:19='99',<INT>,1:18]"
                        + "[@5,20:20='.',<PERIOD>,1:20]");
    }

    /**
     * Decimal again but with a sign.
     */
    @Test
    public void testSignedDecimalLiteral() {
        lexAndCheck("       1 A value -99.9.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:19='-99',<SIGNED_INT>,1:17]"
                        + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                        + "[@5,21:21='9',<INT>,1:21]"
                        + "[@6,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * A string literal with characters from ISO-8859-1.
     */
    @Test
    public void testIso8859LiteralString() {
        lexAndCheck(
                "       1 A value \"����\"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:22='\"����\"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * The case insensitive char reader should allow keywords to be mixed case
     * without impact on lexer grammar.
     */
    @Test
    public void testCaseInsensitiveReader() {
        lexAndCheck("       1 A  REDEFINES b.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:20='REDEFINES',<REDEFINES_KEYWORD>,1:12]"
                        + "[@3,22:22='b',<DATA_NAME>,1:22]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");

        lexAndCheck("       1 A  redeFines b.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:20='redeFines',<REDEFINES_KEYWORD>,1:12]"
                        + "[@3,22:22='b',<DATA_NAME>,1:22]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
    }

    /**
     * Date format case.
     */
    @Test
    public void testDateFormat() {
        lexAndCheck("       1 YY  DATE FORMAT YY.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:10='YY',<DATA_NAME>,1:9]"
                        + "[@2,18:23='FORMAT',<DATE_FORMAT_KEYWORD>,1:18]"
                        + "[@3,25:26='YY',<DATE_PATTERN>,1:25]"
                        + "[@4,27:27='.',<PERIOD>,1:27]");
    }

    /**
     * Date format case with IS.
     */
    @Test
    public void testDateFormatWithIs() {
        lexAndCheck("       1 YY  DATE FORMAT IS YY.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:10='YY',<DATA_NAME>,1:9]"
                        + "[@2,18:23='FORMAT',<DATE_FORMAT_KEYWORD>,1:18]"
                        + "[@3,28:29='YY',<DATE_PATTERN>,1:28]"
                        + "[@4,30:30='.',<PERIOD>,1:30]");
    }

    /**
     * A picture case.
     */
    @Test
    public void testPicture() {
        lexAndCheck("       01 MYVAR PIC 99.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                        + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                        + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                        + "[@4,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * A picture case where the period is separated by a space resulting in
     * lexer identifying the symbol string as an INT.
     */
    @Test
    public void testPictureSeparateFromPeriod() {
        lexAndCheck("       01 MYVAR PIC 99 .",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                        + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                        + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
    }

    /**
     * A picture clause with a decimal point.
     */
    @Test
    public void testPictureWithPeriod() {
        lexAndCheck("       01 MYVAR PIC 99.9.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                        + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                        + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                        + "[@4,22:22='.',<DECIMAL_POINT>,1:22]"
                        + "[@5,23:23='9',<PICTURE_PART>,1:23]"
                        + "[@6,24:24='.',<PERIOD>,1:24]");
    }

    /**
     * A picture clause with a CR or DB type of symbol.
     */
    @Test
    public void testPictureWithDoubleCharacters() {
        lexAndCheck("       01 MYVAR PIC 99.9CR.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                        + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                        + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                        + "[@4,22:22='.',<DECIMAL_POINT>,1:22]"
                        + "[@5,23:25='9CR',<PICTURE_PART>,1:23]"
                        + "[@6,26:26='.',<PERIOD>,1:26]");
    }

    /**
     * More than one statement.
     */
    @Test
    public void testMultipleStatements() {
        lexAndCheck(
                "       01 MYVAR1." + LS + "        02 MYVAR2 PIC X.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:15='MYVAR1',<DATA_NAME>,1:10]"
                        + "[@2,16:16='.',<PERIOD>,1:16][@3,26:27='02',<DATA_ITEM_LEVEL>,2:8]"
                        + "[@4,29:34='MYVAR2',<DATA_NAME>,2:11]"
                        + "[@5,36:38='PIC',<PICTURE_KEYWORD>,2:18]"
                        + "[@6,40:40='X',<PICTURE_PART>,2:22]"
                        + "[@7,41:41='.',<PERIOD>,2:23]");
    }

    /**
     * Multiple statements and a comment.
     */
    @Test
    public void testMultipleStatementsAndComments() {
        lexAndCheck("" + "       01 MYVAR1." + LS + "      * A comment." + LS
                + "           02 MYVAR2 PIC 99.9.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:15='MYVAR1',<DATA_NAME>,1:10]"
                        + "[@2,16:16='.',<PERIOD>,1:16]"
                        + "[@3,30:31='02',<DATA_ITEM_LEVEL>,3:11]"
                        + "[@4,33:38='MYVAR2',<DATA_NAME>,3:14]"
                        + "[@5,40:42='PIC',<PICTURE_KEYWORD>,3:21]"
                        + "[@6,44:45='99',<PICTURE_PART>,3:25]"
                        + "[@7,46:46='.',<DECIMAL_POINT>,3:27]"
                        + "[@8,47:47='9',<PICTURE_PART>,3:28]"
                        + "[@9,48:48='.',<PERIOD>,3:29]");
    }

    /**
     * An hex string literal.
     */
    @Test
    public void testHexLiteralString() {
        lexAndCheck("       1 A value X\"FB\"" + LS,
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:21='X\"FB\"',<HEX_LITERAL_STRING>,1:17]");
    }

    /**
     * An zero terminated string literal.
     */
    @Test
    public void testZeroLiteralString() {
        lexAndCheck("       1 A value Z'q'",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:20='Z'q'',<ZERO_LITERAL_STRING>,1:17]");
    }

    /**
     * An DBCS string literal.
     */
    @Test
    public void testDBCSLiteralString() {
        lexAndCheck("       1 A value G'q'",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:20='G'q'',<DBCS_LITERAL_STRING>,1:17]");
    }

    /**
     * An National string literal.
     */
    @Test
    public void testNationalLiteralString() {
        lexAndCheck("       1 A value N'q'",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:20='N'q'',<NATIONAL_LITERAL_STRING>,1:17]");
    }

    /**
     * An Hex National string literal.
     */
    @Test
    public void testHexNationalLiteralString() {
        lexAndCheck(
                "       1 A value NX'F5F7'",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:24='NX'F5F7'',<NATIONAL_HEX_LITERAL_STRING>,1:17]");
    }

    /**
     * An Decimal numeric literal.
     */
    @Test
    public void testDecimalNumericLiteral() {
        lexAndCheck("       1 A  VALUE 99.9.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:19='99',<INT>,1:18]"
                        + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                        + "[@5,21:21='9',<INT>,1:21]"
                        + "[@6,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * An Float numeric literal.
     */
    @Test
    public void testFloatNumericLiteral() {
        lexAndCheck("       1 A  VALUE 99.9E56.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:19='99',<INT>,1:18]"
                        + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                        + "[@5,21:24='9E56',<FLOAT_PART2>,1:21]"
                        + "[@6,25:25='.',<PERIOD>,1:25]");

        lexAndCheck("       1 A  VALUE IS -0.78E+23.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,21:22='-0',<SIGNED_INT>,1:21]"
                        + "[@4,23:23='.',<DECIMAL_POINT>,1:23]"
                        + "[@5,24:29='78E+23',<FLOAT_PART2>,1:24]"
                        + "[@6,30:30='.',<PERIOD>,1:30]");
    }

    /**
     * Test figurative constants recognition.
     */
    @Test
    public void testFigurativeConstants() {
        lexAndCheck("       1 A VALUE ZERO.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:20='ZERO',<ZERO_CONSTANT>,1:17]"
                        + "[@4,21:21='.',<PERIOD>,1:21]");
        lexAndCheck("       1 A VALUE ZEROS.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:21='ZEROS',<ZERO_CONSTANT>,1:17]"
                        + "[@4,22:22='.',<PERIOD>,1:22]");
        lexAndCheck("       1 A VALUE ZEROES.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:22='ZEROES',<ZERO_CONSTANT>,1:17]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
        lexAndCheck("       1 A  VALUE SPACE.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:22='SPACE',<SPACE_CONSTANT>,1:18]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
        lexAndCheck("       1 A  VALUE SPACES.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:23='SPACES',<SPACE_CONSTANT>,1:18]"
                        + "[@4,24:24='.',<PERIOD>,1:24]");
        lexAndCheck("       1 A  VALUE HIGH-VALUE.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:27='HIGH-VALUE',<HIGH_VALUE_CONSTANT>,1:18]"
                        + "[@4,28:28='.',<PERIOD>,1:28]");
        lexAndCheck("       1 A  VALUE HIGH-VALUES.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:28='HIGH-VALUES',<HIGH_VALUE_CONSTANT>,1:18]"
                        + "[@4,29:29='.',<PERIOD>,1:29]");
        lexAndCheck("       1 A  VALUE LOW-VALUE.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:26='LOW-VALUE',<LOW_VALUE_CONSTANT>,1:18]"
                        + "[@4,27:27='.',<PERIOD>,1:27]");
        lexAndCheck("       1 A  VALUE LOW-VALUES.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:27='LOW-VALUES',<LOW_VALUE_CONSTANT>,1:18]"
                        + "[@4,28:28='.',<PERIOD>,1:28]");
        lexAndCheck("       1 A  VALUE QUOTE.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:22='QUOTE',<QUOTE_CONSTANT>,1:18]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
        lexAndCheck("       1 A  VALUE QUOTES.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:23='QUOTES',<QUOTE_CONSTANT>,1:18]"
                        + "[@4,24:24='.',<PERIOD>,1:24]");
        lexAndCheck("       1 A VALUE ALL 'A'.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                        + "[@3,17:19='ALL',<ALL_CONSTANT>,1:17]"
                        + "[@4,21:23=''A'',<ALPHANUM_LITERAL_STRING>,1:21]"
                        + "[@5,24:24='.',<PERIOD>,1:24]");

        lexAndCheck("       1 A  VALUE NULL.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:21='NULL',<NULL_CONSTANT>,1:18]"
                        + "[@4,22:22='.',<PERIOD>,1:22]");

        lexAndCheck("       1 A  VALUE NULLS.",
                "[@0,7:7='1',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,9:9='A',<DATA_NAME>,1:9]"
                        + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                        + "[@3,18:22='NULLS',<NULL_CONSTANT>,1:18]"
                        + "[@4,23:23='.',<PERIOD>,1:23]");
    }

    /**
     * Helper to test string fragments assembly using a regular expression.
     */
    @Test
    public void testReplace() {
        String regex = "(\\r)?\\n(\\s)*\\-(\\s)*(\"|\')";
        assertEquals("ab", "ab".replaceAll(regex, ""));
        assertEquals("", "\r\n-\"".replaceAll(regex, ""));
        assertEquals("", "\n  -  \"".replaceAll(regex, ""));
        assertEquals("\r\n", "\r\n".replaceAll(regex, ""));
        assertEquals("", "\r\n-\'".replaceAll(regex, ""));
        assertEquals("", "\n  - \'".replaceAll(regex, ""));
        assertEquals("EEEEEEEEEEGGGGGGGGGG",
                "EEEEEEEEEE\r\n      -               \"GGGGGGGGGG".replaceAll(
                        regex, ""));
    }

    /**
     * A condition.
     */
    @Test
    public void testCondition() {
        lexAndCheck("      88 CONDITION VALUE 99.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:26='99',<INT>,1:25]"
                        + "[@4,27:27='.',<PERIOD>,1:27]");
        lexAndCheck("      88 CONDITION VALUE 'A'.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:27=''A'',<ALPHANUM_LITERAL_STRING>,1:25]"
                        + "[@4,28:28='.',<PERIOD>,1:28]");
    }

    /**
     * A condition.
     */
    @Test
    public void testConditionWithFigurativeConstant() {
        lexAndCheck("      88 ISEMPTY VALUE ALL SPACES.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:15='ISEMPTY',<DATA_NAME>,1:9]"
                        + "[@2,17:21='VALUE',<VALUE_KEYWORD>,1:17]"
                        + "[@3,23:25='ALL',<ALL_CONSTANT>,1:23]"
                        + "[@4,27:32='SPACES',<SPACE_CONSTANT>,1:27]"
                        + "[@5,33:33='.',<PERIOD>,1:33]");
        lexAndCheck("      88 INRANGLE VALUE ALL \"A\" THROUGH ALL \"C\".",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:16='INRANGLE',<DATA_NAME>,1:9]"
                        + "[@2,18:22='VALUE',<VALUE_KEYWORD>,1:18]"
                        + "[@3,24:26='ALL',<ALL_CONSTANT>,1:24]"
                        + "[@4,28:30='\"A\"',<ALPHANUM_LITERAL_STRING>,1:28]"
                        + "[@5,32:38='THROUGH',<THROUGH_KEYWORD>,1:32]"
                        + "[@6,40:42='ALL',<ALL_CONSTANT>,1:40]"
                        + "[@7,44:46='\"C\"',<ALPHANUM_LITERAL_STRING>,1:44]"
                        + "[@8,47:47='.',<PERIOD>,1:47]");
    }

    /**
     * A condition with comma separated values.
     */
    @Test
    public void testConditionWithCommaSeparatedValues() {
        lexAndCheck("      88 CONDITION VALUE 1, 2, 3.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:25='1',<INT>,1:25]"
                        + "[@4,28:28='2',<INT>,1:28]"
                        + "[@5,31:31='3',<INT>,1:31]"
                        + "[@6,32:32='.',<PERIOD>,1:32]");
        lexAndCheck("      88 CONDITION VALUE 'A', 'B', 'C'.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:27=''A'',<ALPHANUM_LITERAL_STRING>,1:25]"
                        + "[@4,30:32=''B'',<ALPHANUM_LITERAL_STRING>,1:30]"
                        + "[@5,35:37=''C'',<ALPHANUM_LITERAL_STRING>,1:35]"
                        + "[@6,38:38='.',<PERIOD>,1:38]");
    }

    /**
     * A condition with comma separated values.
     */
    @Test
    public void testConditionWithWhitespaceSeparatedValues() {
        lexAndCheck("      88 CONDITION VALUE 1 2 3.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:25='1',<INT>,1:25]"
                        + "[@4,27:27='2',<INT>,1:27]"
                        + "[@5,29:29='3',<INT>,1:29]"
                        + "[@6,30:30='.',<PERIOD>,1:30]");
        lexAndCheck("      88 CONDITION VALUE 'A' 'B' 'C'.",
                "[@0,6:7='88',<CONDITION_LEVEL>,1:6]"
                        + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                        + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                        + "[@3,25:27=''A'',<ALPHANUM_LITERAL_STRING>,1:25]"
                        + "[@4,29:31=''B'',<ALPHANUM_LITERAL_STRING>,1:29]"
                        + "[@5,33:35=''C'',<ALPHANUM_LITERAL_STRING>,1:33]"
                        + "[@6,36:36='.',<PERIOD>,1:36]");
    }

    /**
     * A value clause followed by a keyword.
     */
    @Test
    public void testValueThenKeyword() {
        lexAndCheck(
                "       01 myName PIC 9 SYNCHRONIZED.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:15='myName',<DATA_NAME>,1:10]"
                        + "[@2,17:19='PIC',<PICTURE_KEYWORD>,1:17]"
                        + "[@3,21:21='9',<PICTURE_PART>,1:21]"
                        + "[@4,23:34='SYNCHRONIZED',<SYNCHRONIZED_KEYWORD>,1:23]"
                        + "[@5,35:35='.',<PERIOD>,1:35]");
    }

    /**
     * A value clause followed by a keyword.
     */
    @Test
    public void testUsageThenKeyword() {
        lexAndCheck(
                "       01 hisName USAGE COMPUTATIONAL-1.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                        + "[@2,18:22='USAGE',<USAGE_KEYWORD>,1:18]"
                        + "[@3,24:38='COMPUTATIONAL-1',<SINGLE_FLOAT_KEYWORD>,1:24]"
                        + "[@4,39:39='.',<PERIOD>,1:39]");

        lexAndCheck("       01 hisName USAGE DISPLAY-1.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                        + "[@2,18:22='USAGE',<USAGE_KEYWORD>,1:18]"
                        + "[@3,24:32='DISPLAY-1',<DISPLAY_1_KEYWORD>,1:24]"
                        + "[@4,33:33='.',<PERIOD>,1:33]");

        lexAndCheck("       01 hisName DISPLAY.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                        + "[@2,18:24='DISPLAY',<DISPLAY_KEYWORD>,1:18]"
                        + "[@3,25:25='.',<PERIOD>,1:25]");
    }

    /**
     * Special separator cases.
     */
    @Test
    public void testSpecialSeparators() {
        lexAndCheck("       01 hisName INDEXED BY A, B.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                        + "[@2,18:24='INDEXED',<INDEXED_KEYWORD>,1:18]"
                        + "[@3,29:29='A',<DATA_NAME>,1:29]"
                        + "[@4,32:32='B',<DATA_NAME>,1:32]"
                        + "[@5,33:33='.',<PERIOD>,1:33]");

        lexAndCheck("       01 hisName VALUE 1, 2.",
                "[@0,7:8='01',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                        + "[@2,18:22='VALUE',<VALUE_KEYWORD>,1:18]"
                        + "[@3,24:24='1',<INT>,1:24]"
                        + "[@4,27:27='2',<INT>,1:27]"
                        + "[@5,28:28='.',<PERIOD>,1:28]");
    }

    /**
     * Test a missing period.
     */
    @Test
    public void testMissingPeriod() {
        lexAndCheck("       10 CUSTOM-NAME               PIC X(\n"
                + "      10 MAX-REPLIES                 PIC S9(4) COMP. ",
                new RecognizerException(
                        "line 2:8 Syntax error in last picture clause"));
    }

    /**
     * Test an invalid symbol.
     */
    @Test
    public void testSyntaxError() {
        lexAndCheck(
                "       10 CUSTOM-NAME               PIC ABEGNPSVXZ0123.456789,+CRD-*$().",
                "[@0,7:8='10',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:20='CUSTOM-NAME',<DATA_NAME>,1:10]"
                        + "[@2,36:38='PIC',<PICTURE_KEYWORD>,1:36]"
                        + "[@3,40:53='ABEGNPSVXZ0123',<PICTURE_PART>,1:40]"
                        + "[@4,54:54='.',<DECIMAL_POINT>,1:54]"
                        + "[@5,55:70='456789,+CRD-*$()',<PICTURE_PART>,1:55]"
                        + "[@6,71:71='.',<PERIOD>,1:71]");

        lexAndCheck(
                "       10 CUSTOM-NAME               ABEGNPSVXZ0123.456789,+CRD-*$().",
                new RecognizerException(
                        "line 1:67 Syntax error in last COBOL clause"));
    }

    /**
     * Test lower case picture symbols.
     */
    @Test
    public void testLowerCasePictureSymbols() {
        lexAndCheck(
                "       10 custom-name               pic x(4).\n"
                        + "      10 max-replies                 pic s9(4) comp. ",
                "[@0,7:8='10',<DATA_ITEM_LEVEL>,1:7]"
                        + "[@1,10:20='custom-name',<DATA_NAME>,1:10]"
                        + "[@2,36:38='pic',<PICTURE_KEYWORD>,1:36]"
                        + "[@3,40:43='x(4)',<PICTURE_PART>,1:40]"
                        + "[@4,44:44='.',<PERIOD>,1:44][@5,52:53='10',<DATA_ITEM_LEVEL>,2:6]"
                        + "[@6,55:65='max-replies',<DATA_NAME>,2:9][@7,83:85='pic',<PICTURE_KEYWORD>,2:37]"
                        + "[@8,87:91='s9(4)',<PICTURE_PART>,2:41]"
                        + "[@9,93:96='comp',<BINARY_KEYWORD>,2:47]"
                        + "[@10,97:97='.',<PERIOD>,2:51]");
    }

    /**
     * Test identifiers starting with digit.
     */
    @Test
    public void testIdentifierStartsWithDigit() {
        lexAndCheck("        01  5500-REC-01.\n"
                + "          05 5500-REC-TYPE      PIC X(01).\n"
                + "          05 5500-PLAN-NUM      PIC X(06).",
                "[@0,8:9='01',<DATA_ITEM_LEVEL>,1:8]"
                        + "[@1,12:22='5500-REC-01',<DATA_NAME>,1:12]"
                        + "[@2,23:23='.',<PERIOD>,1:23]"
                        + "[@3,35:36='05',<DATA_ITEM_LEVEL>,2:10]"
                        + "[@4,38:50='5500-REC-TYPE',<DATA_NAME>,2:13]"
                        + "[@5,57:59='PIC',<PICTURE_KEYWORD>,2:32]"
                        + "[@6,61:65='X(01)',<PICTURE_PART>,2:36]"
                        + "[@7,66:66='.',<PERIOD>,2:41]"
                        + "[@8,78:79='05',<DATA_ITEM_LEVEL>,3:10]"
                        + "[@9,81:93='5500-PLAN-NUM',<DATA_NAME>,3:13]"
                        + "[@10,100:102='PIC',<PICTURE_KEYWORD>,3:32]"
                        + "[@11,104:108='X(06)',<PICTURE_PART>,3:36]"
                        + "[@12,109:109='.',<PERIOD>,3:41]");
    }

    /**
     * Test identifier FORMAT.
     */
    @Test
    public void testIdentifierIsFormat() {
        lexAndCheck("        01  FORMAT.\n",
                "[@0,8:9='01',<DATA_ITEM_LEVEL>,1:8]"
                        + "[@1,12:17='FORMAT',<DATA_NAME>,1:12]"
                        + "[@2,18:18='.',<PERIOD>,1:18]");
    }

    /**
     * Test value containing delimiter.
     */
    @Test
    public void testValueWithDelimiter() {
        lexAndCheck(
                "        10 FILLER  PIC X(56) VALUE 'CONTO N. W '.\n",
                "[@0,8:9='10',<DATA_ITEM_LEVEL>,1:8]"
                        + "[@1,11:16='FILLER',<DATA_NAME>,1:11]"
                        + "[@2,19:21='PIC',<PICTURE_KEYWORD>,1:19]"
                        + "[@3,23:27='X(56)',<PICTURE_PART>,1:23]"
                        + "[@4,29:33='VALUE',<VALUE_KEYWORD>,1:29]"
                        + "[@5,35:47=''CONTO N. W '',<ALPHANUM_LITERAL_STRING>,1:35]"
                        + "[@6,48:48='.',<PERIOD>,1:48]");
    }

    /**
     * Issue #15, long delimiter not detected if newline.
     */
    @Test
    public void testLevel88ValueOnMultipleLines() {
        lexAndCheck(
                "        88 AT31-TRAN-DISPUTE-YES VALUE 'Y',\n        '2' THRU '9'.\n",
                "[@0,8:9='88',<CONDITION_LEVEL>,1:8]"
                        + "[@1,11:31='AT31-TRAN-DISPUTE-YES',<DATA_NAME>,1:11]"
                        + "[@2,33:37='VALUE',<VALUE_KEYWORD>,1:33]"
                        + "[@3,39:41=''Y'',<ALPHANUM_LITERAL_STRING>,1:39]"
                        + "[@4,52:54=''2'',<ALPHANUM_LITERAL_STRING>,2:8]"
                        + "[@5,56:59='THRU',<THROUGH_KEYWORD>,2:12]"
                        + "[@6,61:63=''9'',<ALPHANUM_LITERAL_STRING>,2:17]"
                        + "[@7,64:64='.',<PERIOD>,2:20]");
    }



}
