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

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.BaseRecognizer;
import org.antlr.runtime.EarlyExitException;
import org.antlr.runtime.FailedPredicateException;
import org.antlr.runtime.MismatchedTokenException;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.UnwantedTokenException;
import org.slf4j.Logger;

/**
 * A helper class which does error message formating and also stores
 * all errors reported by recognizers (this is useful for recoverable
 * errors which might be important).
 *
 */
public class RecognizerErrorHandler {

    /** The list of error messages gathered.*/
    private List < String > _errorMessages = new ArrayList < String >();
    
    

    /**
     * Format an error message as expected by ANTLR. It is basically the
     * same error message that ANTL BaseRecognizer generates with some
     * additional data.
     * Also used to log debugging information.
     * @param log the logger to use at debug time
     * @param recognizer the lexer or parser who generated the error
     * @param e the exception that occured
     * @param superMessage the error message that the super class generated
     * @param tokenNames list of token names
     * @return a formatted error message
     */
    public static String getErrorMessage(
            final Logger log,
            final BaseRecognizer recognizer,
            final RecognitionException e,
            final String superMessage,
            final String[] tokenNames) {
        if (log.isDebugEnabled()) {
            List < ? > stack = BaseRecognizer.getRuleInvocationStack(
                    e, recognizer.getClass().getSuperclass().getName());
            String debugMsg = recognizer.getErrorHeader(e)
                + " " + e.getClass().getSimpleName()
                + ": " + superMessage
                + ":";
            if (e instanceof NoViableAltException) {
                NoViableAltException nvae = (NoViableAltException) e;
                debugMsg += " (decision=" + nvae.decisionNumber
                + " state=" + nvae.stateNumber + ")"
                + " decision=<<" + nvae.grammarDecisionDescription + ">>";
            } else if (e instanceof UnwantedTokenException) {
                UnwantedTokenException ute = (UnwantedTokenException) e;
                debugMsg += " (unexpected token=" + toString(ute.getUnexpectedToken(), tokenNames) + ")";

            } else if (e instanceof EarlyExitException) {
                EarlyExitException eea = (EarlyExitException) e;
                debugMsg += " (decision=" + eea.decisionNumber + ")";
            }
            debugMsg += " ruleStack=" + stack.toString();
            log.debug(debugMsg);
        }

        return makeUserMsg(e, superMessage);
    }
    
    /**
     * Simplify error message text for end users.
     * @param e exception that occurred
     * @param msg as formatted by ANTLR
     * @return a more readable error message
     */
    public static String makeUserMsg(final RecognitionException e, final String msg) {
        if (e instanceof NoViableAltException) {
            return msg.replace("no viable alternative at", "unrecognized");
        } else if (e instanceof UnwantedTokenException) {
            return msg.replace("extraneous input", "unexpected token");
        } else if (e instanceof MismatchedTokenException) {
            if (msg.contains("mismatched input '<EOF>'")) {
                return msg.replace("mismatched input '<EOF>' expecting", "reached end of file looking for");
            } else {
                return msg.replace("mismatched input", "unexpected token");
            }
        } else if (e instanceof EarlyExitException) {
            return msg.replace("required (...)+ loop did not match anything", "required tokens not found");
        } else if (e instanceof FailedPredicateException) {
            if (msg.contains("picture_string failed predicate: {Unbalanced parentheses}")) {
                return "Unbalanced parentheses in picture string";
            }
            if (msg.contains("PICTURE_PART failed predicate: {Contains invalid picture symbols}")) {
                return "Picture string contains invalid symbols";
            }
            if (msg.contains("PICTURE_PART failed predicate: {Syntax error in last picture clause}")) {
                return "Syntax error in last picture clause";
            }
            if (msg.contains("DATA_NAME failed predicate: {Syntax error in last clause}")) {
                return "Syntax error in last COBOL clause";
            }
        }
        return msg;
    }

    /**
     * @param msg the last error message recorded
     */
    public void addMessageToHistory(final String msg) {
        getErrorMessages().add(msg);
    }

    /**
     * Token traces are slightly more readable if numeric type is translated to a readable string.
     * @param token a lexer token
     * @param tokenNames the list of token names
     * @return same as Token.toString but with token type label rather than int
     */
    public static String toString(final Token token, final String[] tokenNames) {
        return token.toString().replace("<" + token.getType() + ">",
                "<" + ((token.getType() > -1) ? tokenNames[token.getType()] : "no type") + ">");
    }

    /**
     * @return the error message history list
     */
    public List < String > getErrorMessages() {
        return _errorMessages;
    }

}
