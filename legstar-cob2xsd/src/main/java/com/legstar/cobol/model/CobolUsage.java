/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobol.model;

/**
 * These are the COBOL usage attribute that are derived from the original COBOL
 * statement and then propagated as attributes in XML Schema and java
 * annototations.
 * 
 * @author Fady Moussallam
 * 
 */
public final class CobolUsage {

    /**
     * Utility class.
     */
    private CobolUsage() {

    }

    /** An character string. */
    public static final String DISPLAY = "DISPLAY";
    /** An double byte character string. */
    public static final String DISPLAY_1 = "DISPLAY-1";
    /** A UTF16-BE character string. */
    public static final String NATIONAL = "NATIONAL";
    /** A binary numeric. */
    public static final String BINARY = "BINARY";
    /** A native binary numeric. */
    public static final String COMP_5 = "COMP-5";
    /** A packed numeric. */
    public static final String PACKED_DECIMAL = "COMP-3";
    /** A single float. */
    public static final String COMP_1 = "COMP-1";
    /** A double float. */
    public static final String COMP_2 = "COMP-2";
    /** An index. */
    public static final String INDEX = "INDEX";
    /** A pointer. */
    public static final String POINTER = "POINTER";
    /** A pointer to a procedure. */
    public static final String PROCEDURE_POINTER = "PROCEDURE-POINTER";
    /** A pointer to a function. */
    public static final String FUNCTION_POINTER = "FUNCTION-POINTER";

    /** Usable as java enumeration. */
    public enum Usage {
        /** COMP. */
        BINARY,
        /** COMP-1. */
        SINGLEFLOAT,
        /** COMP-2. */
        DOUBLEFLOAT,
        /** COMP-3. */
        PACKEDDECIMAL,
        /** COMP-5. */
        NATIVEBINARY,
        /** DISPLAY. */
        DISPLAY,
        /** DBCS. */
        DISPLAY1,
        /** INDEX. */
        INDEX,
        /** UTF-16. */
        NATIONAL,
        /** Pointer. */
        POINTER,
        /** Procedure pointer. */
        PROCEDUREPOINTER,
        /** Function pointer. */
        FUNCTIONPOINTER
    };

    /**
     * @param usage the java enumeration
     * @return the Cobol usage clause (as a COBOL string)
     */
    public static String getCobolUsage(Usage usage) {
        if (usage == null) {
            return null;
        }
        switch (usage) {
        case BINARY:
            return BINARY;
        case SINGLEFLOAT:
            return COMP_1;
        case DOUBLEFLOAT:
            return COMP_2;
        case PACKEDDECIMAL:
            return PACKED_DECIMAL;
        case NATIVEBINARY:
            return COMP_5;
        case DISPLAY:
            return DISPLAY;
        case DISPLAY1:
            return DISPLAY_1;
        case INDEX:
            return INDEX;
        case NATIONAL:
            return NATIONAL;
        case POINTER:
            return POINTER;
        case PROCEDUREPOINTER:
            return PROCEDURE_POINTER;
        case FUNCTIONPOINTER:
            return FUNCTION_POINTER;
        default:
            throw new IllegalArgumentException("Unknown Enum usage: " + usage);

        }
    }

    /**
     * Get the java enumeration corresponding to a COBOL usage string.
     * 
     * @param cobolUsage the COBOL usage string
     * @return the java enumeration
     */
    public static Usage getUsage(String cobolUsage) {
        if (cobolUsage == null) {
            return null;
        }
        if (cobolUsage.equals(BINARY)) {
            return Usage.BINARY;
        } else if (cobolUsage.equals("COMP")) {
            return Usage.BINARY;
        } else if (cobolUsage.equals("COMPUTATIONAL")) {
            return Usage.BINARY;
        } else if (cobolUsage.equals("COMP-4")) {
            return Usage.BINARY;
        } else if (cobolUsage.equals("COMPUTATIONAL-4")) {
            return Usage.BINARY;
        } else if (cobolUsage.equals(COMP_1)) {
            return Usage.SINGLEFLOAT;
        } else if (cobolUsage.equals(COMP_2)) {
            return Usage.DOUBLEFLOAT;
        } else if (cobolUsage.equals(PACKED_DECIMAL)) {
            return Usage.PACKEDDECIMAL;
        } else if (cobolUsage.equals("PACKED-DECIMAL")) {
            return Usage.PACKEDDECIMAL;
        } else if (cobolUsage.equals("COMPUTATIONAL-3")) {
            return Usage.PACKEDDECIMAL;
        } else if (cobolUsage.equals(COMP_5)) {
            return Usage.NATIVEBINARY;
        } else if (cobolUsage.equals("COMPUTATIONAL-5")) {
            return Usage.NATIVEBINARY;
        } else if (cobolUsage.equals(DISPLAY)) {
            return Usage.DISPLAY;
        } else if (cobolUsage.equals(DISPLAY_1)) {
            return Usage.DISPLAY1;
        } else if (cobolUsage.equals(INDEX)) {
            return Usage.INDEX;
        } else if (cobolUsage.equals(NATIONAL)) {
            return Usage.NATIONAL;
        } else if (cobolUsage.equals(POINTER)) {
            return Usage.POINTER;
        } else if (cobolUsage.equals(PROCEDURE_POINTER)) {
            return Usage.PROCEDUREPOINTER;
        } else if (cobolUsage.equals(FUNCTION_POINTER)) {
            return Usage.FUNCTIONPOINTER;
        } else {
            throw new IllegalArgumentException("Unknown COBOL usage: "
                    + cobolUsage);
        }

    }

}
