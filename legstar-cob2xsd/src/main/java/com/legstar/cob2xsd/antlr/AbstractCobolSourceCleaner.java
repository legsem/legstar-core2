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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * In order to reduce the lexer/parser grammar complexity, this class will
 * remove all unnecessary characters from the original source. This way, the
 * ANTLR lexer will be presented with a purified source that only contains data
 * division entries.
 * <p/>
 * This allows users to submit complete COBOL programs or fragments of COBOL
 * programs with non data description statements to the parser without the need
 * to add grammar rules for all these cases.
 * 
 */
public abstract class AbstractCobolSourceCleaner {

    /** Cobol sentences are assumed to be terminated by this character. */
    public static final char COBOL_DELIMITER = '.';

    /** Pattern that recognizes the start of a data description entry. */
    public static final Pattern DATA_DESCRIPTION_START = Pattern
            .compile("(^|\\s|\\" + COBOL_DELIMITER + ")\\d(\\d)?(\\s|\\"
                    + COBOL_DELIMITER + "|$)");

    /** Pattern that recognizes the end of a data description entry. */
    public static final Pattern DATA_DESCRIPTION_END = Pattern.compile("(\\"
            + COBOL_DELIMITER + "$)|(\\" + COBOL_DELIMITER + "\\s)");

    /** Pattern that recognizes the start of an alphanumeric literal. */
    public static final Pattern ALPHANUM_LITERAL_START = Pattern
            .compile("(^|\\s)[\\\"\']");

    /** Pattern that recognizes the start of a procedure division. */
    public static final Pattern PROCEDURE_DIVISION = Pattern.compile(
            "^(\\s)*PROCEDURE DIVISION", Pattern.CASE_INSENSITIVE);

    /** Pattern that recognizes the start of an identification division. */
    public static final Pattern IDENTIFICATION_DIVISION = Pattern.compile(
            "^(\\s)*ID(ENTIFICATION)? DIVISION", Pattern.CASE_INSENSITIVE);

    /** Pattern that recognizes the start of a data division. */
    public static final Pattern DATA_DIVISION = Pattern.compile(
            "^(\\s)*DATA DIVISION", Pattern.CASE_INSENSITIVE);

    /**
     * List of compiler directives (they can be period delimited but are
     * guaranteed to be alone on a line).
     */
    public static final List < String > COMPILER_DIRECTIVES = Arrays.asList(
            "EJECT", "SKIP", "SKIP1", "SKIP2", "SKIP3");

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler;

    /**
     * Construct with a shared error handler.
     * 
     * @param errorHandler handles error messages
     */
    public AbstractCobolSourceCleaner(final RecognizerErrorHandler errorHandler) {
        _errorHandler = errorHandler;

    }

    /**
     * Takes in a raw COBOL source, potentially containing sequence numbers or
     * non data description statements and produces a clean source code.
     * <p/>
     * Statements which are not data descriptions become empty lines in order to
     * preserve original line numbering.
     * 
     * @param cobolReader a reader for the raw COBOL source
     * @return the source cleaned up
     * @throws CleanerException if source cannot be read
     */
    public String clean(final Reader cobolReader) throws CleanerException {

        if (cobolReader == null) {
            throw new CleanerException("COBOL source was null");
        }

        BufferedReader reader = new BufferedReader(cobolReader);
        String line;
        StringBuilder cleanedSource = new StringBuilder();
        CleaningContext context = new CleaningContext();
        try {
            while ((line = reader.readLine()) != null) {
                if (isLineOfCode(line) && isDataDivision(line, context)) {
                    cleanedSource.append(removeExtraneousCharacters(
                            cleanLine(line), context));
                }
                cleanedSource.append("\n");
            }
            if (cleanedSource.length() <= "\n".length()) {
                throw new CleanerException(
                        "No data descriptions found. Are you sure this is COBOL source?");
            }
            return cleanedSource.toString();
        } catch (IOException e) {
            throw new CleanerException(e);
        }
    }

    /**
     * Make sure this is a line worth parsing. Ignore empty lines, comments and
     * compiler directives.
     * 
     * @param line the line to parse
     * @return true if this is not an empty or comment line
     */
    public boolean isLineOfCode(final String line) {
        if (line.length() < getIndicatorAreaPos() + 1) {
            return false;
        }

        /* Remove white space lines */
        if (line.trim().length() == 0) {
            return false;
        }

        /* Remove comments and special lines */
        if (isComment(line)) {
            return false;
        }

        /*
         * If there is a single token on this line, make sure it is not a
         * compiler directive.
         */
        String[] tokens = line.trim().split("[\\s\\.]+");
        if (tokens.length == 1
                && COMPILER_DIRECTIVES.contains(tokens[0].toUpperCase(Locale
                        .getDefault()))) {
            return false;
        }

        return true;
    }

    /**
     * Remove characters that should not be passed to the lexer.
     * 
     * @param line before cleaning
     * @return a cleaner line of code
     */
    public String cleanLine(final String line) {

        String cleanedLine = extendedCleanLine(line);

        /* Right trim, no need to over burden the lexer with spaces */
        cleanedLine = ("a" + cleanedLine).trim().substring(1);
        return cleanedLine;
    }

    /**
     * Specialized cleaners determine if this line is a comment.
     * 
     * @param line the line to check
     * @return true if this line is a comment
     */
    public abstract boolean isComment(final String line);

    /**
     * Derived classes can extend this method to further clean a line of code.
     * 
     * @param line the current line of code
     * @return a cleaner line of code
     */
    public String extendedCleanLine(final String line) {
        return line;
    }

    /**
     * Replace token separators such as ", " and "; " which complicate matters
     * uselessly. Replacement should not change column numbers though so we
     * simply replace the extra separators with a whitespace.
     * 
     * @param str a string containing long separators
     * @return the same string where long sperators have been replaced with
     *         white spaces
     */
    protected String replaceLongSeparators(String str) {
        return str.replace(", ", "  ").replace("; ", "  ");
    }

    /**
     * Rough triage of statements which are not strictly part of the data
     * division. Detects end of DATA DIVISION by looking for PROCEDURE DIVISION.
     * <p/>
     * Since we are not guaranteed to have identification division, we initially
     * consider we are in the data division. If we find an identification
     * division, then we stop processing till we find a data division.
     * 
     * @param line the line to set data description status from
     * @param context the data description detection context
     * @return true if we are within the data division
     */
    public boolean isDataDivision(final String line,
            final CleaningContext context) {
        if (context.isDataDivision()) {
            Matcher matcher = IDENTIFICATION_DIVISION.matcher(line);
            if (matcher.find()) {
                context.setDataDivision(false);
                emitErrorMessage("Found identification division in ["
                        + line.trim() + "]. Lines ignored till data division.");
            }
            matcher = PROCEDURE_DIVISION.matcher(line);
            if (matcher.find()) {
                context.setDataDivision(false);
                emitErrorMessage("Found procedure division in [" + line.trim()
                        + "]. Remaining lines ignored.");
            }
        } else {
            Matcher matcher = DATA_DIVISION.matcher(line);
            if (matcher.find()) {
                context.setDataDivision(true);
                emitErrorMessage("Found data division in [" + line.trim()
                        + "]. Started looking for data items.");
            }
        }
        return context.isDataDivision();
    }

    /**
     * @return the zero-based position of the indicator area
     */
    public abstract int getIndicatorAreaPos();

    /**
     * Removes characters which are not part of a data description entry.
     * <p/>
     * The fragment received as a parameter is assumed to be cleaned from
     * sequence numbers.
     * <p/>
     * Data description entries start with an integer (the level) and end with a
     * period followed by either space, newline or EOF.
     * <p/>
     * A single line might hold multiple data descriptions. This method is
     * recursive, and is called multiple times for each line fragment holding a
     * new data description.
     * <p/>
     * Data description entries might span multiple lines which is why we need
     * to keep a context. Context tells us if we need to start by looking for a
     * level (no data description has started on some previous line) or for a
     * period.
     * <p/>
     * Unsupported data description instructions such as COPY might appear on
     * the same line as data instructions. They also can span multiple lines.
     * This code blanks out such "non data description" statements.
     * <p/>
     * Code that is outside alphanumeric literals is also cleaned from long
     * separators.
     * 
     * @param fragment a fragment of a line which might hold a data description
     * @param context the data description detection context
     * @return a line holding only data description parts or blank
     */
    public String removeExtraneousCharacters(final String fragment,
            final CleaningContext context) {
        if (fragment == null || fragment.length() == 0) {
            return fragment;
        }
        Matcher matcher;
        Matcher alphanumLiteralMatcher;
        StringBuilder cleanedLine = new StringBuilder();
        if (context.isLookingForLevel()) {
            matcher = DATA_DESCRIPTION_START.matcher(fragment);
            if (matcher.find()) {

                /*
                 * if the level does not start on the first character, the regex
                 * starts on the space or period character that precedes the
                 * level.
                 */
                int start = (matcher.start() > 0) ? matcher.start() + 1
                        : matcher.start();

                int endClean = start;

                if (start > 0) {
                    /*
                     * If there are non blank characters before the level, make
                     * sure they are period terminated otherwise assume this is
                     * not a level but more likely an argument for a keyword. In
                     * this last case, we need to clean the argument as well as
                     * all the previous characters.
                     */
                    if (isArgument(fragment.substring(0, start))) {
                        endClean = matcher.end() - 1;
                    }

                    /*
                     * Warn that we are about to get rid of these extra
                     * characters.
                     */
                    String extraneous = fragment.substring(0, endClean).trim();
                    if (extraneous.length() > 0) {
                        emitErrorMessage("Extraneous characters ignored: "
                                + extraneous);
                    }
                }

                /* Any extraneous character is replaced with spaces. */
                for (int i = 0; i < endClean; i++) {
                    cleanedLine.append(' ');
                }

                /*
                 * If we actually found a level, keep it and start looking for a
                 * delimiter.
                 */
                if (endClean == start) {
                    cleanedLine.append(fragment.substring(start,
                            matcher.end() - 1));
                    context.setLookingForLevel(false);
                }

                cleanedLine.append(removeExtraneousCharacters(
                        fragment.substring(matcher.end() - 1), context));
            } else {
                if (fragment.trim().length() > 0) {
                    emitErrorMessage("Extraneous characters ignored: "
                            + fragment);
                }
            }
        } else if (context.isAlphanumStarted()) {
            Pattern alphanumLiteralEnd = Pattern.compile("\\"
                    + context.getAlphanumDelimiter() + "($|\\s|,|;|\\"
                    + COBOL_DELIMITER + ")");
            alphanumLiteralMatcher = alphanumLiteralEnd.matcher(fragment);
            if (alphanumLiteralMatcher.find()) {
                cleanedLine.append(fragment.substring(0,
                        alphanumLiteralMatcher.end() - 1));
                context.setAlphanumStarted(false);
                cleanedLine.append(removeExtraneousCharacters(
                        fragment.substring(alphanumLiteralMatcher.end() - 1),
                        context));
            } else {
                cleanedLine.append(fragment);
            }
        } else {
            alphanumLiteralMatcher = ALPHANUM_LITERAL_START.matcher(fragment);
            matcher = DATA_DESCRIPTION_END.matcher(fragment);
            if (alphanumLiteralMatcher.find()) {
                if (matcher.find()) {
                    if (matcher.end() < alphanumLiteralMatcher.end()) {
                        cleanedLine.append(replaceLongSeparators(fragment
                                .substring(0, matcher.end())));
                        context.setLookingForLevel(true);
                        cleanedLine.append(removeExtraneousCharacters(
                                fragment.substring(matcher.end()), context));
                    } else {
                        cleanedLine.append(replaceLongSeparators(fragment
                                .substring(0, alphanumLiteralMatcher.end())));
                        context.setAlphanumDelimiter(fragment.substring(
                                alphanumLiteralMatcher.end() - 1).charAt(0));
                        context.setAlphanumStarted(true);
                        cleanedLine.append(removeExtraneousCharacters(fragment
                                .substring(alphanumLiteralMatcher.end()),
                                context));
                    }
                } else {
                    cleanedLine.append(replaceLongSeparators(fragment
                            .substring(0, alphanumLiteralMatcher.end())));
                    context.setAlphanumDelimiter(fragment.substring(
                            alphanumLiteralMatcher.end() - 1).charAt(0));
                    context.setAlphanumStarted(true);
                    cleanedLine.append(removeExtraneousCharacters(
                            fragment.substring(alphanumLiteralMatcher.end()),
                            context));
                }
            } else {
                if (matcher.find()) {
                    cleanedLine.append(replaceLongSeparators(fragment
                            .substring(0, matcher.end())));
                    context.setLookingForLevel(true);
                    cleanedLine.append(removeExtraneousCharacters(
                            fragment.substring(matcher.end()), context));
                } else {
                    cleanedLine.append(fragment);
                }

            }
        }
        return cleanedLine.toString();
    }

    /**
     * Describes the cleaning context. Because data description sentences can be
     * multiline or because it does not make sense to look for data description
     * entries once we past a PROCEDURE DIVISION section, we need to keep track
     * of the context.
     * 
     */
    public static class CleaningContext {

        /**
         * True when we are looking for a level (start of a data description
         * entry).
         */
        private boolean _lookingForLevel = true;

        /** True if we are likely to be in a COBOL DATA DIVISION section. */
        private boolean _inDataDivision = true;

        /**
         * Will be true when an alphanumeric delimiter is found and not yet
         * closed.
         */
        private boolean _alphanumStarted;

        /** When an alphanumeric is started this is the delimiter character. */
        private char _alphanumDelimiter;

        /**
         * @return true when we are looking for a level
         */
        public boolean isLookingForLevel() {
            return _lookingForLevel;
        }

        /**
         * @param isLookingForLevel set to true when we are looking for a level
         *            (start of a data description entry)
         */
        public void setLookingForLevel(final boolean isLookingForLevel) {
            _lookingForLevel = isLookingForLevel;
        }

        /**
         * @return true if we are likely to be in a COBOL DATA DIVISION section
         */
        public boolean isDataDivision() {
            return _inDataDivision;
        }

        /**
         * @param dataDivision set to true if we are likely to be in a COBOL
         *            DATA DIVISION section
         */
        public void setDataDivision(final boolean dataDivision) {
            _inDataDivision = dataDivision;
        }

        /**
         * @return true when an alphanumeric delimiter is found and not yet
         *         closed
         */
        public boolean isAlphanumStarted() {
            return _alphanumStarted;
        }

        /**
         * @param alphanumStarted true when an alphanumeric delimiter is found
         *            and not yet closed
         */
        public void setAlphanumStarted(boolean alphanumStarted) {
            this._alphanumStarted = alphanumStarted;
        }

        /**
         * @return when an alphanumeric is started this is the delimiter
         *         character
         */
        public char getAlphanumDelimiter() {
            return _alphanumDelimiter;
        }

        /**
         * @param alphanumDelimiter when an alphanumeric is started this is the
         *            delimiter character
         */
        public void setAlphanumDelimiter(char alphanumDelimiter) {
            this._alphanumDelimiter = alphanumDelimiter;
        }

    }

    /**
     * Examine characters before an assumed level. If these characters are not
     * terminated by a COBOL delimiter then the level is actually an argument to
     * a previous keyword, not an actual level.
     * 
     * @param fragment a fragment of code preceding an assumed level
     * @return true if the assumed level is an argument
     */
    protected boolean isArgument(final String fragment) {
        String s = fragment.trim();
        if (s.length() > 0) {
            return s.charAt(s.length() - 1) != COBOL_DELIMITER;
        }
        return false;
    }

    /**
     * Add an error message to the history.
     * 
     * @param msg the error message
     * */
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
