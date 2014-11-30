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

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.tree.TreeNodeStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cobol.CobolStructureEmitter;


/**
 * Overrides some of the ANTLR generated lexer methods so that the resulting
 * java class behaves like other LegStar classes, particularly for logging purposes.
 * <p/>
 * This code could be imbedded in the lexer grammar as well but its harder to
 * debug using ANTLRWorks because this code might have dependencies on jars which
 * are not naturally in ANTLRWorks classpath.
 */
public class CobolStructureEmitterImpl extends CobolStructureEmitter {

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(CobolStructureEmitterImpl.class);

    /** Handles error messages.*/
    private RecognizerErrorHandler _errorHandler;

    /**
     * Construct from a tree nodes stream.
     * @param input the tree nodes stream
     * @param errorHandler handles error messages
     */
    public CobolStructureEmitterImpl(
            final TreeNodeStream input,
            final RecognizerErrorHandler errorHandler) {
        this(input, new RecognizerSharedState(), errorHandler);
    }

    /**
     * Construct from a tree nodes stream and a shared state.
     * @param input the tree nodes stream
     * @param state the shared state
     * @param errorHandler handles error messages
     */
    public CobolStructureEmitterImpl(
            final TreeNodeStream input,
            final RecognizerSharedState state,
            final RecognizerErrorHandler errorHandler) {
        super(input, state);
        _errorHandler = errorHandler;
    }
    
    /** {@inheritDoc} */
    public String getErrorMessage(final RecognitionException e, final String[] tokenNames) {
        return RecognizerErrorHandler.getErrorMessage(
                _log, this, e, super.getErrorMessage(e, tokenNames), tokenNames);
    }

    /** {@inheritDoc} */
    public void emitErrorMessage(final String msg) {
        getErrorHandler().addMessageToHistory(msg);
    }

    /**
     * @return the error messages handler
     */
    public RecognizerErrorHandler getErrorHandler() {
        return _errorHandler;
    }
}
