/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
lexer grammar CobolStructureLexer;

/*------------------------------------------------------------------
 * Imaginary nodes
 *------------------------------------------------------------------*/
tokens {
    DECIMAL_POINT ;
    DATA_ITEM_LEVEL;
    RENAMES_LEVEL;
    CONDITION_LEVEL;
/*------------------------------------------------------------------
 * COBOL Structures keywords
 *------------------------------------------------------------------*/
    RENAMES_KEYWORD; 
    THROUGH_KEYWORD; 
    REDEFINES_KEYWORD; 
    BLANK_KEYWORD; 
    WHEN_KEYWORD; 
    EXTERNAL_KEYWORD; 
    GLOBAL_KEYWORD; 
    GROUP_USAGE_KEYWORD; 
    IS_KEYWORD; 
    ARE_KEYWORD; 
    NATIONAL_KEYWORD; 
    JUSTIFIED_KEYWORD; 
    RIGHT_KEYWORD; 
    OCCURS_KEYWORD; 
    TIMES_KEYWORD; 
    TO_KEYWORD; 
    ASCENDING_KEYWORD; 
    DESCENDING_KEYWORD; 
    KEY_KEYWORD; 
    INDEXED_KEYWORD; 
    BY_KEYWORD; 
    PICTURE_KEYWORD; 
    DEPENDING_KEYWORD; 
    ON_KEYWORD; 
    SIGN_KEYWORD; 
    SIGN_LEADING_KEYWORD; 
    SIGN_TRAILING_KEYWORD; 
    SEPARATE_KEYWORD; 
    CHARACTER_KEYWORD; 
    SYNCHRONIZED_KEYWORD; 
    LEFT_KEYWORD; 
    USAGE_KEYWORD; 
    SINGLE_FLOAT_KEYWORD; 
    DOUBLE_FLOAT_KEYWORD; 
    NATIVE_BINARY_KEYWORD; 
    PACKED_DECIMAL_KEYWORD; 
    BINARY_KEYWORD; 
    DISPLAY_1_KEYWORD; 
    DISPLAY_KEYWORD; 
    INDEX_KEYWORD; 
    POINTER_KEYWORD; 
    PROCEDURE_POINTER_KEYWORD; 
    FUNCTION_POINTER_KEYWORD; 
    VALUE_KEYWORD; 
    DATE_KEYWORD; 
    DATE_FORMAT_KEYWORD; 
 
/*------------------------------------------------------------------
 * Figurative constants
 * ZERO_CONSTANT is also a keyword in BLANK WHEN ZERO
 *------------------------------------------------------------------*/
    ZERO_CONSTANT; 
    SPACE_CONSTANT; 
    HIGH_VALUE_CONSTANT; 
    LOW_VALUE_CONSTANT; 
    QUOTE_CONSTANT; 
    ALL_CONSTANT; 
    NULL_CONSTANT; 
}
/*------------------------------------------------------------------
 * Lexer grammar for COBOL structures.
 * Built from a subset of IBM Entreprise COBOL V3R4
 *------------------------------------------------------------------*/

/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.cobol;
import java.util.HashMap;
import java.util.Map;
}

@members {
    /** Keeps track of the last COBOL keyword recognized. This helps
        disambiguate lexing rules. */
    private int lastKeyword = PERIOD;
    
    /** True when a picture string is being built (potentially from multiple parts). */
    private boolean pictureStarted;

    /** Map to help with COBOL keyword recognition.*/
    private static Map <String, Integer> KEYWORDS_MAP = new HashMap <String, Integer>()
    {{
        put("RENAMES", RENAMES_KEYWORD);
        put("THROUGH", THROUGH_KEYWORD);
        put("THRU", THROUGH_KEYWORD);
        put("REDEFINES", REDEFINES_KEYWORD);
        put("BLANK", BLANK_KEYWORD);
        put("WHEN", Token.SKIP_TOKEN.getType());
        put("EXTERNAL", EXTERNAL_KEYWORD);
        put("GLOBAL", GLOBAL_KEYWORD);
        put("GROUP-USAGE", GROUP_USAGE_KEYWORD);
        put("IS", Token.SKIP_TOKEN.getType());
        put("ARE", Token.SKIP_TOKEN.getType());
        put("NATIONAL", NATIONAL_KEYWORD);
        put("JUSTIFIED", JUSTIFIED_KEYWORD);
        put("JUST", JUSTIFIED_KEYWORD);
        put("RIGHT", RIGHT_KEYWORD);
        put("OCCURS", OCCURS_KEYWORD);
        put("TIMES", Token.SKIP_TOKEN.getType());
        put("TO", TO_KEYWORD);
        put("ASCENDING", ASCENDING_KEYWORD);
        put("DESCENDING", DESCENDING_KEYWORD);
        put("KEY", KEY_KEYWORD);
        put("INDEXED", INDEXED_KEYWORD);
        put("BY", Token.SKIP_TOKEN.getType());
        put("PICTURE", PICTURE_KEYWORD);
        put("PIC", PICTURE_KEYWORD);
        put("DEPENDING", DEPENDING_KEYWORD);
        put("ON", Token.SKIP_TOKEN.getType());
        put("SIGN", Token.SKIP_TOKEN.getType());
        put("LEADING", SIGN_LEADING_KEYWORD);
        put("TRAILING", SIGN_TRAILING_KEYWORD);
        put("SEPARATE", SEPARATE_KEYWORD);
        put("CHARACTER", Token.SKIP_TOKEN.getType());
        put("SYNCHRONIZED", SYNCHRONIZED_KEYWORD);
        put("SYNC", SYNCHRONIZED_KEYWORD);
        put("LEFT", LEFT_KEYWORD);
        put("USAGE", USAGE_KEYWORD);
        put("COMPUTATIONAL-1", SINGLE_FLOAT_KEYWORD);
        put("COMP-1", SINGLE_FLOAT_KEYWORD);
        put("COMPUTATIONAL-2", DOUBLE_FLOAT_KEYWORD);
        put("COMP-2", DOUBLE_FLOAT_KEYWORD);
        put("COMPUTATIONAL-5", NATIVE_BINARY_KEYWORD);
        put("COMP-5", NATIVE_BINARY_KEYWORD);
        put("COMPUTATIONAL-3", PACKED_DECIMAL_KEYWORD);
        put("COMP-3", PACKED_DECIMAL_KEYWORD);
        put("PACKED-DECIMAL", PACKED_DECIMAL_KEYWORD);
        put("COMPUTATIONAL", BINARY_KEYWORD);
        put("COMP", BINARY_KEYWORD);
        put("COMPUTATIONAL-4", BINARY_KEYWORD);
        put("COMP-4", BINARY_KEYWORD);
        put("BINARY", BINARY_KEYWORD);
        put("DISPLAY-1", DISPLAY_1_KEYWORD);
        put("DISPLAY", DISPLAY_KEYWORD);
        put("INDEX", INDEX_KEYWORD);
        put("POINTER", POINTER_KEYWORD);
        put("PROCEDURE-POINTER", PROCEDURE_POINTER_KEYWORD);
        put("FUNCTION-POINTER", FUNCTION_POINTER_KEYWORD);
        put("VALUES", VALUE_KEYWORD);
        put("VALUE", VALUE_KEYWORD);
        put("DATE", DATE_KEYWORD);
        put("FORMAT", DATE_FORMAT_KEYWORD);
        put("ZEROES", ZERO_CONSTANT);
        put("ZEROS", ZERO_CONSTANT);
        put("ZERO", ZERO_CONSTANT);
        put("SPACES", SPACE_CONSTANT);
        put("SPACE", SPACE_CONSTANT);
        put("HIGH-VALUES", HIGH_VALUE_CONSTANT);
        put("HIGH-VALUE", HIGH_VALUE_CONSTANT);
        put("LOW-VALUES", LOW_VALUE_CONSTANT);
        put("LOW-VALUE", LOW_VALUE_CONSTANT);
        put("QUOTES", QUOTE_CONSTANT);
        put("QUOTE", QUOTE_CONSTANT);
        put("ALL", ALL_CONSTANT);
        put("NULLS", NULL_CONSTANT);
        put("NULL", NULL_CONSTANT);
    }};
    
    /**
     * Adding all the COBOL keywords directly generates too much code. It
     * is more efficient, with the current release of ANTLR, to recognize
     * keywords manually.
     * @param text the text to match with keywords
     * @param originalType the initial token type
     * @return the keyword type if a match is found otherwise the original type
     */
    public int matchKeywords(
            final String text,
            final int originalType) {
        Integer type = KEYWORDS_MAP.get(text.toUpperCase());
        if (type == null) {
            return originalType;
        }
        if (type == Token.SKIP_TOKEN.getType()) {
            skip();
            return type;
        } else if(type == PICTURE_KEYWORD) {
            /* Just found a PICTURE keyword, start collecting picture string parts */
            pictureStarted = true;
        } else if(type == DATE_KEYWORD) {
            skip();
        } else if(type == DATE_FORMAT_KEYWORD) {
            /* Format is a date format only if preceded by date */
            if (lastKeyword != DATE_KEYWORD) {
                type = DATA_NAME;
            }
        }
        lastKeyword = type;
        return type;
    }
        
    /**
     * Check that a string is a valid data name.
     * @param string a proposed data name
     * @throws FailedPredicateException if this is not a valid data name
     */
    public void checkDataName(final String string) throws FailedPredicateException {
        if (!string.matches("[a-zA-Z0-9][a-zA-Z0-9_\\-]*")) {
            throw new FailedPredicateException(
                    input, "DATA_NAME", "Syntax error in last clause");
        }
    }

    /**
     * Is this a separator.
     * @param string a proposed separator
     * @return true if this is a separator
     */
    public boolean isSeparator(final String string) {
        return string.matches("\\s*[,|;]\\s*");
    }

    /**
     * Check that a string is a valid part of a picture string.
     * <p>
     * Check that we are in the context of collecting picture string parts.
     * <p>
     * When string is a valid picture part, close picture string sequence
     * if the next character is space or new line.
     * 
     * @param string a picture string or part of a picture string
     * @throws FailedPredicateException if this is not a valid picture string part
     */
    public void checkPicture(final String string) throws FailedPredicateException {
        if (!pictureStarted) {
            throw new FailedPredicateException(
                    input, "PICTURE_PART", "Syntax error in last picture clause");
        }
        if (input.LA(1) == ' ' || input.LA(1) == '\r' || input.LA(1) == '\n' || input.LA(1) == -1) {
            pictureStarted = false;
        }
    }

}
/*------------------------------------------------------------------
 * Lexer grammar
 *------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Period is the data entry delimiter.
 * It might also appear in a PICTURE clause, FLOAT or DECIMAL literal.
 * Fortunately in these cases, it cannot appear as the last character.
 * The action here detects these cases and dynamically retype the
 * token produced by the lexer.
 *------------------------------------------------------------------*/
PERIOD
    :
    {
        /* If this period is not followed by a space or a newline, then we consider
         * it is a decimal point and not to be used as a sentence delimiter.*/
        if (input.LA(2) != ' ' && input.LA(2) != '\r' && input.LA(2) != '\n' && input.LA(2) != -1) {
            $type = DECIMAL_POINT;
        } else {
            /* This will set the context as the end of a data entry */
            lastKeyword = PERIOD;
        }
    } '.'
    ;

/*------------------------------------------------------------------
 * Integer literals
 * We may have to retype these tokens which have a broad pattern,
 * depending on context.
 *------------------------------------------------------------------*/
INT :   '0'..'9'+ 
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            checkPicture(getText());
            $type = PICTURE_PART;
        }
        if (lastKeyword == PERIOD) {
            int level = Integer.parseInt($text);
            if (level == 66) {
                $type = RENAMES_LEVEL;
            } else if (level == 88) {
                $type = CONDITION_LEVEL;
            } else {
                $type = DATA_ITEM_LEVEL;
            }
        }
    }
    ;

SIGNED_INT
    : ('+' | '-') '0'..'9'+ 
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            checkPicture(getText());
            $type = PICTURE_PART;
        }
    }
    ;

/*------------------------------------------------------------------
 * Floating point literals are fragmented because DECIMAL_POINT
 * occurs in the mantissa. The first part of the floating point is
 * recognized as an INT or SIGNED_INT and the second part, wich holds
 * at least one digit of the mantissa decimal part and the following
 * exponent, is recognized here.
 *------------------------------------------------------------------*/
FLOAT_PART2
    : '0'..'9'+ 'E' ('+' | '-')? '0'..'9'+ 
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            checkPicture(getText());
            $type = PICTURE_PART;
        }
    }
    ;

/*------------------------------------------------------------------
 * Date pattern
 * A date pattern such as XX is ambiguous for the lexer because it
 * can also be a DATA_NAME or a PICTURE_STRING. By declaring
 * DATE_PATTERN first we hush the lexer complaining. But now, 
 * everytime the lexer encounters XX it will assume a DATE_PATTERN
 * The post action retypes the token according to context.
 *------------------------------------------------------------------*/
DATE_PATTERN
    : ('X'|'Y')+ 
    {
        if (lastKeyword != DATE_FORMAT_KEYWORD) {
            if (lastKeyword == PICTURE_KEYWORD) {
                checkPicture(getText());
                $type = PICTURE_PART;
            } else {
               $type = DATA_NAME;
            }
        }
    }
    ;

/*------------------------------------------------------------------
 * Data item names
 * A data name such as ABE is ambiguous because it might as well be
 * a PICTURE_STRING. We retype the token if that's the case.
 * All COBOL keywords fall into this category. Since COBOL keywords
 * are reserved and cannot be used as DATA_NAME, we check with an
 * auxiliary parser for a match and change the token type accordingly.
 *------------------------------------------------------------------*/
DATA_NAME
    : (LETTER|'0'..'9') (LETTER|'0'..'9'|'-'|'_')*
    {
        $type = matchKeywords(getText(), $type);
        if ($type == DATA_NAME) {
            if (lastKeyword == PICTURE_KEYWORD) {
                checkPicture(getText());
                $type = PICTURE_PART;
            }
        }
    }
    ;

/*------------------------------------------------------------------
 * Picture value
 * This might be a part from a picture value in case a decimal point
 * is detected. For a PICTURE such as 99.9 the lexer will recognize
 * 3 tokens : PICTURE_STRING DECIMAL_POINT PICTURE_STRING.
 * The complete picture string is reconstructed by the 
 * picture_string parser rule.
 * If we were not looking for a picture, we might be on a value
 * separator (which we ignore) or on a data name
 *------------------------------------------------------------------*/
PICTURE_PART
    : PICTURE_CHAR+
    {
        if (lastKeyword != PICTURE_KEYWORD) {
            if (isSeparator(getText())) {
              skip();
            } else {
              checkDataName(getText());
              $type = DATA_NAME;
            }
        } else {
            checkPicture(getText());
        }
    }
    ;
    
/*------------------------------------------------------------------
 * Although picture characters are normally taken from a limited set,
 * the currency symbol can be pretty much any character.
 * 
 * In addition a PICTURE can also contain a DECIMAL point. We have to
 * exclude it here though because the lexer would get confused
 * because period is also the sentence delimiter.
 *------------------------------------------------------------------*/
fragment
PICTURE_CHAR
    : ~( QUOTE | APOST | SPACE | '\r' | '\n' | '.')
    ;

/*------------------------------------------------------------------
 * Alphanumeric literals are delimited by QUOTE or APOST
 * Escaping is done by duplicating the delimiter. For instance,
 * "aaa""bbbb" is a valid COBOL literal string.
 * Strings can be continued on multiple lines in which case:
 * - The continued line does not terminate with a delimiter
 * - The continuation line has a '-' in column 7
 * when we concatenate fragments from multiple lines, we end up with
 * things like "aaa\n  - "bbb" which we manually clean up to become
 * "aaabbb"
 *------------------------------------------------------------------*/
ALPHANUM_LITERAL_STRING
    :   ALPHANUM_LITERAL_FRAGMENT+
    {setText(getText().replaceAll("(\\r)?\\n(\\s)*\\-(\\s)*(\"|\')",""));}
    ;

fragment
ALPHANUM_LITERAL_FRAGMENT
    :   QUOTE (options {greedy=false;} : .)* ( QUOTE | CONTINUED_ALPHANUM_LITERAL_FRAGMENT)
    |   APOST (options {greedy=false;} : .)* ( APOST | CONTINUED_ALPHANUM_LITERAL_FRAGMENT)
    ;

fragment
CONTINUED_ALPHANUM_LITERAL_FRAGMENT
  :  ('\r'? '\n') SPACE+ CONTINUATION_CHAR SPACE* ALPHANUM_LITERAL_FRAGMENT+
  ;

fragment
CONTINUATION_CHAR
    :   {getCharPositionInLine() == 6}?=> '-'
    ;
    
/*------------------------------------------------------------------
 * Hexadecimal literal strings
 *------------------------------------------------------------------*/
HEX_LITERAL_STRING
    :   'X' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * Zero terminated literal strings
 *------------------------------------------------------------------*/
ZERO_LITERAL_STRING
    :   'Z' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * DBCS literal strings
 *------------------------------------------------------------------*/
DBCS_LITERAL_STRING
    :   'G' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * National literal strings
 *------------------------------------------------------------------*/
NATIONAL_LITERAL_STRING
    :   'N' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * National hexadecimal literal strings
 *------------------------------------------------------------------*/
NATIONAL_HEX_LITERAL_STRING
    :   'NX' ALPHANUM_LITERAL_STRING
    ;


/*------------------------------------------------------------------
 * Whitespaces are not needed by the parser
 *------------------------------------------------------------------*/
WHITESPACE
    :   SPACE+ { skip(); }
    ;

/*------------------------------------------------------------------
 * Newlines are not needed by the parser
 *------------------------------------------------------------------*/
NEWLINE
    :   ('\r'? '\n')+  { skip(); }
    ;


/*------------------------------------------------------------------
 * Fragments
 *------------------------------------------------------------------*/
fragment LETTER     : 'A'..'Z'| 'a'..'z';
fragment SPACE      : ' ' | '\t';
fragment QUOTE      : '"';
fragment APOST      : '\'';
