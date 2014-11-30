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
package com.legstar.cobol.utils;

import java.util.Locale;

/**
 * A set of utility methods to manipulate COBOL values.
 *
 */
public final class ValueUtil {

    /**
     * Utility class.
     */
    private ValueUtil() {
        
    }

    /**
     * Translate individual figurative constants.
     * @param value a potential figurative constant value
     * @param length the target data item length
     * @param quoteIsQuote true if quote is quote, false if quote is apost
     * @return a translated value or the value stripped from delimiters if it is not a  figurative constant
     */
    public static String resolveFigurative(
            final String value,
            final int length,
            final boolean quoteIsQuote) {

        if (value == null || value.length() == 0) {
            return value;
        }
        String ucValue = value.toUpperCase(Locale.getDefault());

        if (ucValue.matches("^ZERO(S|ES)?$")) {
            return "0";
        }

        /* We avoid filling with spaces because this is the most common
         * initial value for large strings*/
        if (ucValue.matches("^SPACES?$")) {
            return " ";
        }

        if (ucValue.matches("^QUOTES?$")) {
            return quoteIsQuote ? "\"" : "\'";
        }
        if (ucValue.matches("^APOST$")) {
            return "\'";
        }

        /* For binary content, we use pseudo hexadecimal representation
         * This is understood downstream by the COBOL binder. */
        if (ucValue.matches("^HIGH-VALUES?$")) {
            return fill("0x", "FF", length);
        }
        if (ucValue.matches("^LOW-VALUES?$")) {
            return fill("0x", "00", length);
        }
        /* Nulls are treated like low-value. */
        if (ucValue.matches("^NULLS?$")) {
            return fill("0x", "00", length);
        }
        
        /* All is followed by a literal which can be a figurative constant.
         * If that figurative constant is binary, then the filling already
         * took place. Otherwise, if the total length is not a multiple
         * of the repeated character sequence, then we need to fill the
         * the remainder with a substring of the literal. */
        if (ucValue.startsWith("ALL ")) {
            String literal = value.substring("ALL ".length());
            String resolvedLiteral = ValueUtil.resolveFigurative(
                    literal, length, quoteIsQuote);
            if (resolvedLiteral.length() > 0 && resolvedLiteral.length() < length) {
                String filled = fill(null, resolvedLiteral, length / resolvedLiteral.length());
                if (filled.length() < length) {
                    return filled + resolvedLiteral.substring(0, length - filled.length());
                } else {
                    return filled;
                }
            } else {
                return resolvedLiteral;
            }
        }

        /* This is not a figurative constant just strip delimiters */
        return stripDelimiters(value);
    }
    

    /**
     * Create a string representation repeating a character
     * sequence the requested number of times.
     * @param prefix an optional prefix to prepend
     * @param charSequence the character sequence to repeat
     * @param times how many times the character sequence should be repeated
     * @return a string filled with character sequence or empty string
     */
    public static String fill(
            final String prefix,
            final String charSequence,
            final int times) {
        if (times < 1) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        if (prefix != null) {
            sb.append(prefix);
        }
        for (int i = 0; i < times; i++) {
            sb.append(charSequence);
        }
        return sb.toString();
    }

    /**
     * The parser does not strip delimiters from literal strings so we
     * do it here if necessary.
     * @param value a potential literal string
     * @return the literal without delimiters
     */
    public static String stripDelimiters(final String value) {
        if (value != null && value.length() > 1) {
            if (value.charAt(0) == value.charAt(value.length() - 1)
                    && (value.charAt(0) == '\'')) {
                return value.substring(1, value.length() - 1);
            }
            if (value.charAt(0) == value.charAt(value.length() - 1)
                    && (value.charAt(0) == '\"')) {
                return value.substring(1, value.length() - 1);
            }
        }
        return value;
    }

}
