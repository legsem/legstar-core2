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

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.TreeNodeStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cobol.CobolStructureEmitter;
import com.legstar.cobol.CobolStructureParser;
import com.legstar.cobol.CobolStructureParser.cobdata_return;
import com.legstar.cobol.model.CobolDataItem;

import static org.junit.Assert.*;

/**
 * Generic test code for ANTLR based lexers parsers and tree walkers.
 *
 */
public abstract class AbstractCobolTester extends AbstractAntlrTester {

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(AbstractCobolTester.class);

    /** Handles error messages.*/
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * {@inheritDoc}
     * @throws CleanerException 
     */
    public String clean(final String source) throws CleanerException {
        if (source == null) {
            throw new CleanerException("COBOL source was null");
        }
        CobolFixedFormatSourceCleaner cleaner = new CobolFixedFormatSourceCleaner(getErrorHandler(), 7, 72);
        return cleaner.clean(new StringReader(source));
    }

    /**
     * {@inheritDoc}
     */
    public CommonTokenStream lex(final String source) throws RecognizerException {
        try {
            CobolStructureLexerImpl lex = new CobolStructureLexerImpl(
                    new ANTLRReaderStream(
                            new StringReader(
                                    clean(source))),
                                    getErrorHandler());
            CommonTokenStream tokens = new CommonTokenStream(lex);
            assertTrue(tokens != null);
            return tokens;
        } catch (IOException e) {
            throw new RecognizerException(e);
        }
    }
    
    /**
     * Apply a lexer to a source and check that the token stream produced
     * is as expected.
     * @param source the source code
     * @param expected the expected token stream
     */
    public void lexAndCheck(final String source, final String expected) {
        try {
            CommonTokenStream ts = lex(source);
            ts.fill();
            StringBuilder sb = new StringBuilder();
            for (Object token : ts.getTokens()) {
                sb.append(toString((Token) token));
            }
            CobolStructureLexerImpl lexer = (CobolStructureLexerImpl) ts.getTokenSource();
            if (lexer.getErrorHandler().getErrorMessages().size() > 0) {
                throw new RecognizerException(
                        lexer.getErrorHandler().getErrorMessages().get(0));
            }
            assertEquals(expected, sb.toString());
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * A generic test helper that takes a source fragment and checks the result
     * when it should be an exception.
     * @param source the source fragment
     * @param expected the expected exception
     */
    public void lexAndCheck(
            final String source,
            final RecognizerException expected) {
        try {
            CommonTokenStream ts = lex(source);
            ts.fill();
            CobolStructureLexerImpl lexer = (CobolStructureLexerImpl) ts.getTokenSource();
            if (lexer.getErrorHandler().getErrorMessages().size() > 0) {
                throw new RecognizerException(
                        lexer.getErrorHandler().getErrorMessages().get(0));
            }
            fail();
        } catch (RecognizerException e) {
            assertEquals(expected.getMessage(), e.getMessage());
        }
    }
    /**
     * {@inheritDoc}
     */
    public CommonTree parse(final String source) throws RecognizerException {
        try {
            CommonTokenStream tokens = lex(source);
            CobolStructureParserImpl parser = new CobolStructureParserImpl(
                    tokens, getErrorHandler());
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() > 0) {
                throw new RecognizerException(
                        parser.getErrorHandler().getErrorMessages().get(0));
            }
            assertTrue(parserResult != null);
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * Starting from a COBOL source fragment translates to XML Schema.
     * @param source COBOL source fragment.
     * @return an XML Schema
     * @throws RecognizerException if emit fails
     */
    public String emit(final String source)  throws RecognizerException {
        try {
            CommonTree ast = parse(source);
            if (_log.isDebugEnabled()) {
                _log.debug(ast.toStringTree());
            }
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            CobolStructureEmitter emitter = new CobolStructureEmitterImpl(
                    nodes, getErrorHandler());
            List < CobolDataItem > dataEntries = new ArrayList < CobolDataItem >();
            emitter.cobdata(dataEntries);
            return dataEntries.toString();
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    public String[] getTokenNames() {
        return CobolStructureParser.tokenNames;
    }

    /**
     * @return the error messages handler
     */
    public RecognizerErrorHandler getErrorHandler() {
        return _errorHandler;
    }
}
