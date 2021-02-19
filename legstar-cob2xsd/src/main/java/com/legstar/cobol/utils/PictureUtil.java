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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legstar.cob2xsd.PictureSymbol;

/**
 * Utility class provides methods to introspect COBOL picture clauses.
 *
 */
public final class PictureUtil {

    /**
     * Utility class.
     */
    private PictureUtil() {

    }

    /**
     * Determines how many times a given character occurs in a picture string. A
     * character can appear standalone or as a factored sequence like X(nn).
     * Unlike all other picture symbols, currency symbols are case sensitive.
     * For example, &apos;D&apos; and &apos;d&apos; specify different currency
     * symbols.
     * 
     * @param picture the picture string
     * @param currencySymbol the currency symbol
     * @return a map of all characters to search for
     */
    public static Map < Character, Integer > getPictureCharOccurences(
            final String picture, final char currencySymbol) {

        Map < Character, Integer > charNum = new HashMap < Character, Integer >();
        charNum.put('A', 0);
        charNum.put('B', 0);
        charNum.put('G', 0);
        charNum.put('N', 0);
        charNum.put('X', 0);
        charNum.put('P', 0);
        charNum.put('Z', 0);
        charNum.put('0', 0);
        charNum.put('/', 0);
        charNum.put('+', 0);
        charNum.put('-', 0);
        charNum.put('*', 0);
        charNum.put('C', 0);
        charNum.put('D', 0);
        charNum.put('.', 0);
        charNum.put(',', 0);
        charNum.put('9', 0);
        charNum.put('E', 0);
        charNum.put('S', 0);
        charNum.put('V', 0);
        charNum.put(currencySymbol, 0);

        List < PictureSymbol > pictureSymbols = parsePicture(picture,
                currencySymbol);
        for (PictureSymbol pictureSymbol : pictureSymbols) {
            Integer number = charNum.get(pictureSymbol.getSymbol());
            if (number != null) {
                number += pictureSymbol.getNumber();
                charNum.put(pictureSymbol.getSymbol(), number);
            }
        }

        return charNum;
    }

    /**
     * The COBOL picture clause determines the length, in number of characters,
     * for all alphanumeric and numeric-edited data items.
     * <p>
     * The length evaluated here is either the number of character positions
     * (which corresponds to the size constraint on the client side) or the byte
     * size of the storage needed on z/OS for the data item. You select between
     * one or the other with the calcStorageLength parameter.
     * <p>
     * When the currency sign is more than a single character, then the first
     * occurrence of the currency symbol counts for more than one byte of
     * storage.
     * 
     * @param charNum map of all characters in the picture string
     * @param isSignSeparate if sign occupies a separated position (no
     *            overpunch)
     * @param currencySign the currency sign
     * @param currencySymbol the currency symbol
     * @param calcStorageLength when true the length returned is the z/OS
     *            storage length
     * @return the length, in number of characters, of the data item
     */
    public static int calcLengthFromPicture(
            final Map < Character, Integer > charNum,
            final boolean isSignSeparate, final String currencySign,
            final char currencySymbol, final boolean calcStorageLength) {

        int length = 0;

        /* character position occupied by each picture symbol */
        Map < Character, Integer > charLen = new HashMap < Character, Integer >();
        charLen.put('A', 1);
        charLen.put('B', 1);
        charLen.put('G', (calcStorageLength) ? 2 : 1);
        charLen.put('N', (calcStorageLength) ? 2 : 1);
        charLen.put('X', 1);
        charLen.put('P', 0);
        charLen.put('Z', 1);
        charLen.put('0', 1);
        charLen.put('/', 1);
        charLen.put('+', 1);
        charLen.put('-', 1);
        charLen.put('*', 1);
        charLen.put('C', 2);
        charLen.put('D', 2);
        charLen.put('.', 1);
        charLen.put(',', 1);
        charLen.put('9', 1);
        charLen.put('E', 1);
        charLen.put('S', (isSignSeparate) ? 1 : 0);
        charLen.put('V', 0);
        charLen.put(currencySymbol, 1);

        for (Map.Entry < Character, Integer > entry : charNum.entrySet()) {
            length += entry.getValue() * charLen.get(entry.getKey());
        }
        if (currencySign.length() > 1 && charNum.get(currencySymbol) > 1) {
            length += currencySign.length() - 1;
        }
        return length;
    }

    /**
     * Try to infer a regular expression to match a COBOL picture clause.
     * <p>
     * The objective is to build a string that would fit the internal
     * representation of a picture edited COBOL field.
     * <p>
     * If a picture is not restrictive, for instance PIC X does not impose any
     * restriction, then we return null (no pattern).
     * <p>
     * Regular expressions in XML Schema are more like PERL than Java regex.
     * 
     * @param picture the picture clause
     * @param currencySign the currency sign
     * @param currencySymbol the currency symbol
     * @return a regular expression
     */
    public static String getRegexFromPicture(final String picture,
            final String currencySign, final char currencySymbol) {
        StringBuilder result = new StringBuilder();

        /* Table that associate a picture symbol to a regex atom */
        Map < Character, String > charRegex = new HashMap < Character, String >();
        charRegex.put('A', "[\\p{L}\\s]"); // any letter or space character
        charRegex.put('B', "\\s"); // space
        charRegex.put('G', "."); // TODO does not reflect the double byte nature
        charRegex.put('N', "."); // TODO does not reflect the double byte nature
        charRegex.put('X', "."); // Any byte
        charRegex.put('P', "[\\d\\.]"); // Floating decimal point
        charRegex.put('Z', "[1-9\\s]"); // Numeric or space
        charRegex.put('0', "0"); // Zero character
        charRegex.put('/', "/"); // Forward slash character
        charRegex.put('+', "[\\+\\-\\d]"); // Position can be a sign or a digit
        charRegex.put('-', "[\\+\\-\\d]"); // Position can be a sign or a digit
        charRegex.put('*', "[1-9\\*]"); // Position can be an asterisk or a
                                        // digit
        charRegex.put('C', "(CR|\\s\\s)"); // Credit or spaces
        charRegex.put('D', "(DB|\\s\\s)"); // Debit or spaces
        charRegex.put('.', "\\."); // Decimal point character
        charRegex.put(',', ","); // Comma character
        charRegex.put('9', "\\d"); // A digit
        charRegex.put('E', "E"); // Exponent
        charRegex.put('S', "[\\+\\-]"); // A numeric sign
        charRegex.put('V', ""); // A virtual decimal point
        charRegex.put(currencySymbol, "(" + currencySign.replace(" ", "\\s")
                + "|\\d|\\s)");

        List < PictureSymbol > pictureSymbols = parsePicture(picture,
                currencySymbol);

        /* If there is only one symbol and it is non restrictive, no pattern */
        if (pictureSymbols.size() == 1) {
            String symbol = charRegex.get(pictureSymbols.get(0).getSymbol());
            if (symbol == null || symbol.equals(".")) {
                return null;
            }
        }

        /* Add quantifiers */
        for (PictureSymbol pictureSymbol : pictureSymbols) {
            String regex = charRegex.get(pictureSymbol.getSymbol());
            if (charRegex != null) {
                result.append(regex);
                int occurs = pictureSymbol.getNumber();
                if (occurs > 1) {
                    result.append("{0," + occurs + "}");
                } else {
                    result.append("?");
                }
            }
        }

        return result.toString();
    }

    /**
     * Parse a COBOL picture clause. Character symbols are returned in the order
     * where they are found in the picture clause. All factoring is resolved and
     * each character is associated with its occurrence number.
     * <p>
     * For instance: 9(3)V99XX becomes 4 entries in the list for characters 9,
     * V, 9 and X. First 9 occurs 3 times, V occurs 1 time, 9 occurs 2 and X
     * occurs 2.
     * 
     * @param currencySymbol the currency symbol
     * @param picture the COBOL picture clause
     * @return ordered list of symbols appearing in the picture clause with
     *         their number of occurrences.
     */
    public static List < PictureSymbol > parsePicture(final String picture,
            final char currencySymbol) {
        int factoredNumber = 0;
        boolean factorSequence = false;
        char lastChar = 0;
        PictureSymbol pictureSymbol = null;
        List < PictureSymbol > result = new LinkedList < PictureSymbol >();
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            if (c != currencySymbol) {
                c = Character.toUpperCase(c);
            }
            if (factorSequence) {
                if (c == ')') {
                    pictureSymbol.setNumber(pictureSymbol.getNumber()
                            + factoredNumber - 1);
                    factorSequence = false;
                } else {
                    if (Character.isDigit(c)) {
                        factoredNumber = factoredNumber * 10
                                + Character.getNumericValue(c);
                    }
                }
            } else {
                if (c == '(') {
                    factoredNumber = 0;
                    factorSequence = true;
                } else {
                    /*
                     * CR and DB are special cases where we need to ignore, the
                     * second character R or B.
                     */
                    if ((c != 'B' || lastChar != 'D')
                            && (c != 'R' || lastChar != 'C')) {
                        if (c == lastChar) {
                            pictureSymbol
                                    .setNumber(pictureSymbol.getNumber() + 1);
                        } else {
                            pictureSymbol = new PictureSymbol(c, 1);
                            result.add(pictureSymbol);
                            lastChar = c;
                        }
                    }
                }
            }
        }
        return result;
    }

}
