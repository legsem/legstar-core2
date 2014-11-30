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


import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.Tree;
import org.antlr.stringtemplate.StringTemplate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;

/**
 * Generic test code for ANTLR based lexers parsers and tree walkers.
 * 
 */
public abstract class AbstractAntlrTester {

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(AbstractAntlrTester.class);

    /** Line separator (OS specific). */
    public static final String LS = "\n";

    /**
     * Cleanup source an compare to expected.
     * 
     * @param source original source
     * @param expected expected result
     */
    public void cleanAndCheck(final String source, final String expected) {
        try {
            assertEquals(expected, clean(source));
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }

    }

    /**
     * Apply a lexer to a source and check that the token stream produced is as
     * expected.
     * 
     * @param source the source code
     * @param expected the expected token stream
     */
    public void lexAndCheck(final String source, final String expected) {
        try {
            CommonTokenStream ts = lex(source);
            StringBuilder sb = new StringBuilder();
            for (Object token : ts.getTokens()) {
                sb.append(toString((Token) token));
            }
            assertEquals(expected, sb.toString());
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * 
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    public void parseAndCheck(final String source, final String expected) {
        try {
            CommonTree ast = parse(source);
            assertEquals(expected, (ast == null) ? "" : ast.toStringTree());
            if (_log.isDebugEnabled()) {
                _log.debug(getGraph(ast).toString());
            }
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * A generic test helper that takes a source fragment and checks the result
     * when it should be an exception.
     * 
     * @param source the source fragment
     * @param expected the expected exception
     */
    public void parseAndCheck(final String source,
            final RecognizerException expected) {
        try {
            parse(source);
            fail();
        } catch (RecognizerException e) {
            assertEquals(expected.getMessage(), e.getMessage());
        }
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * 
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    public void emitAndCheck(final String source, final String expected) {
        try {
            String result = emit(source);
            if (_log.isDebugEnabled()) {
                _log.debug("Emitted content");
                _log.debug(result);
            }
            assertEquals(expected, result);
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * @param token a lexer token
     * @return same as Token.toString but with token type label rather than int
     */
    protected String toString(final Token token) {
        if (token.getType() == -1) {
            return ""; // EOF token started showing up in Antlr 3.5.2
        }
        return token.toString().replace("<" + token.getType() + ">",
                "<" + getTokenNames()[token.getType()] + ">");
    }

    /**
     * Produce a dot source for an abstract syntax tree.
     * 
     * @param ast the abstract syntax tree
     * @return a dot source
     */
    private String getGraph(final Tree ast) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(ast);
        return st.toString();
    }

    /**
     * Perform initial source cleanup to keep ANLR grammar simple.
     * 
     * @param source original source code
     * @return cleaned up source code
     * @throws RecognizerException if source cannot be read
     */
    public abstract String clean(final String source)
            throws RecognizerException;

    /**
     * Apply the lexer to produce a token stream from source.
     * 
     * @param source the source code
     * @return an antlr token stream
     * @throws RecognizerException if lexer fails
     */
    public abstract CommonTokenStream lex(final String source)
            throws RecognizerException;

    /**
     * Apply Lexer + Parser to produce an abstract syntax tree from source.
     * 
     * @param source the source code
     * @return an antlr abstract syntax tree
     * @throws RecognizerException if parser fails
     */
    public abstract CommonTree parse(final String source)
            throws RecognizerException;

    /**
     * Apply Lexer + Parser + Emitter to produce some translated content.
     * 
     * @param source the source code
     * @return a translated result
     * @throws RecognizerException if emit fails
     */
    public abstract String emit(final String source) throws RecognizerException;

    /**
     * @return the parser token names (nicer looking than integer types)
     */
    public abstract String[] getTokenNames();
}
