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

import java.io.File;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

/**
 * Check recognition on a complete program source.
 *
 */
public class CompleteProgramsParserTest extends AbstractCobolTester {

    /** Location of COBOL samples.*/
    private File sampleFolder = new File("src/test/cobol");

    /**
     * Try the LSFILEAE sample.
     * @throws Exception if test fails
     */
    @Test
    public void testLsfileae() throws Exception {
        parseAndCheck(
                FileUtils.readFileToString(new File(sampleFolder, "LSFILEAE"), StandardCharsets.UTF_8)
                , "(DATA_ITEM (LEVEL 01) (NAME FILEA))"
                + " (DATA_ITEM (LEVEL 77) (NAME RESPONSE) (PICTURE S9(8)))"
                + " (DATA_ITEM (LEVEL 01) (NAME DFHCOMMAREA)"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-NUMBER) (PICTURE 9(6)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-PERSONAL)"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-NAME) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-ADDRESS) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-PHONE) (PICTURE X(8))))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-DATE) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-AMOUNT) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-COMMENT) (PICTURE X(9))))"
        );
    }

    /**
     * Try the DPLARCHT sample.
     * @throws Exception if test fails
     */
    @Test
    public void testDplarcht() throws Exception {
        parseAndCheck(
                FileUtils.readFileToString(new File(sampleFolder, "DPLARCHT"), StandardCharsets.UTF_8)
                , "(DATA_ITEM (LEVEL 01) (NAME WS-MAX-ITEMS) (PICTURE 9(9)) (USAGE NATIVEBINARY) (VALUE 500))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-MAX-ITEMS-D) (PICTURE 9(9)) (VALUE ZERO))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-ERROR-STATUS) (PICTURE 9(4)) (USAGE BINARY) (VALUE ZERO)"
                + " (CONDITION (LEVEL 88) (NAME NO-ERRORS) (LITERAL ZERO))"
                + " (CONDITION (LEVEL 88) (NAME ERRORS) (LITERAL 1)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-ITEMS-SELECTION) (PICTURE 9(4)) (USAGE BINARY) (VALUE ZERO)"
                + " (CONDITION (LEVEL 88) (NAME LIMITED-ITEMS) (LITERAL ZERO))"
                + " (CONDITION (LEVEL 88) (NAME ALL-ITEMS) (LITERAL 1)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-SEARCH-STATUS) (PICTURE 9(4)) (USAGE BINARY) (VALUE ZERO)"
                + " (CONDITION (LEVEL 88) (NAME SEARCH-ENDED) (LITERAL ZERO))"
                + " (CONDITION (LEVEL 88) (NAME SEARCH-CONTINUE) (LITERAL 1))"
                + " (CONDITION (LEVEL 88) (NAME SEARCH-FOUND) (LITERAL 2)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-ERROR-DESCRIPTION) (PICTURE X(256)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-FILE-DESCRIPTION)"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-FILE-START) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-FILE-NAME) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-FILE-DSNAME) (PICTURE X(44)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-FILE-ENABLESTATUS) (PICTURE 9(8)) (USAGE BINARY) (VALUE ZERO)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-PROGRAM-DESCRIPTION)"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-START) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-NAME) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-TYPE) (PICTURE 9(8)) (USAGE BINARY) (VALUE ZERO))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-LANGUAGE) (PICTURE 9(8)) (USAGE BINARY) (VALUE ZERO))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-LENGTH) (PICTURE S9(9)) (USAGE BINARY))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-PROGRAM-USECOUNT) (PICTURE S9(9)) (USAGE BINARY)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-TRANSACTION-DESCRIPTION)"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-TRANSACTION-START) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-TRANSACTION-NAME) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-TRANSACTION-PROGRAM) (PICTURE X(8)) (VALUE SPACES))"
                + " (DATA_ITEM (LEVEL 05) (NAME WS-TRANSACTION-STATUS) (PICTURE 9(8)) (USAGE BINARY) (VALUE ZERO)))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-RESP) (PICTURE S9(8)) (USAGE BINARY) (VALUE ZERO))"
                + " (DATA_ITEM (LEVEL 01) (NAME WS-RESP2) (PICTURE S9(8)) (USAGE BINARY) (VALUE ZERO))"
                + " (DATA_ITEM (LEVEL 01) (NAME DFHCOMMAREA)"
                + " (DATA_ITEM (LEVEL 05) (NAME LS-REQUEST)"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-REQUEST-TYPE) (PICTURE 9(4)) (USAGE BINARY)"
                + " (CONDITION (LEVEL 88) (NAME FILES-REQUESTED) (LITERAL 0))"
                + " (CONDITION (LEVEL 88) (NAME PROGRAMS-REQUESTED) (LITERAL 1))"
                + " (CONDITION (LEVEL 88) (NAME TRANSACTIONS-REQUESTED) (RANGE 2 9999)))"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-ALL-ITEMS) (PICTURE X(4)))"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-MAX-ITEMS) (REDEFINES LS-ALL-ITEMS) (PICTURE 9(4)))"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-SEARCH-CRITERIA)"
                + " (DATA_ITEM (LEVEL 15) (NAME LS-STARTWITH) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 15) (NAME LS-STARTWITH-LEN) (PICTURE 9(9)) (USAGE PACKEDDECIMAL))))"
                + " (DATA_ITEM (LEVEL 05) (NAME LS-REPLY)"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-REPLY-TYPE) (PICTURE 9(4)) (USAGE BINARY)"
                + " (CONDITION (LEVEL 88) (NAME ITEMS-FOUND) (LITERAL 0))"
                + " (CONDITION (LEVEL 88) (NAME NO-ITEMS-FOUND) (LITERAL 1))"
                + " (CONDITION (LEVEL 88) (NAME ERROR-FOUND) (LITERAL 2)))"
                + " (DATA_ITEM (LEVEL 10) (NAME LS-REPLY-DATA)"
                + " (DATA_ITEM (LEVEL 15) (NAME LS-ITEMS-COUNT) (PICTURE 9(9)) (USAGE NATIVEBINARY))"
                + " (DATA_ITEM (LEVEL 15) (NAME LS-ITEMS-ARRAY)"
                +             " (VARARRAY (LBOUND 1) (HBOUND 500 (DEPENDINGON LS-ITEMS-COUNT)))"
                + " (DATA_ITEM (LEVEL 20) (NAME LS-FILES-DATA)"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-FILE-NAME) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-FILE-DSNAME) (PICTURE X(44)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-FILE-ENABLESTATUS) (PICTURE X(12))))"
                + " (DATA_ITEM (LEVEL 20) (NAME LS-PROGRAMS-DATA) (REDEFINES LS-FILES-DATA)"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-PROGRAM-NAME) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-PROGRAM-TYPE) (PICTURE X(12)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-PROGRAM-LANGUAGE) (PICTURE X(12)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-PROGRAM-LENGTH) (PICTURE S9(9)) (USAGE BINARY))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-PROGRAM-USECOUNT) (PICTURE S9(9)) (USAGE BINARY))"
                + " (DATA_ITEM (LEVEL 25) (PICTURE X(24))))"
                + " (DATA_ITEM (LEVEL 20) (NAME LS-TRANSACTIONS-DATA) (REDEFINES LS-FILES-DATA)"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-TRANSACTION-NAME) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-TRANSACTION-PROGRAM) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 25) (NAME LS-TRANSACTION-STATUS) (PICTURE X(12)))"
                + " (DATA_ITEM (LEVEL 25) (PICTURE X(36))))))))"
                + " (DATA_ITEM (LEVEL 01) (NAME LS-ERROR-DESCRIPTION) (PICTURE X(256)))"
        );
    }

}
