package com.legstar.converter.generator;

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

/**
 * This is the list of all the cobol item types that are supported.
 *
 * 
 */
public enum CobolTypes {
    /** A complex element. */ 
    GROUP_ITEM,
    /** Contains only alphabetic characters. */ 
    ALPHABETIC_ITEM,
    /** A UTF-16 character string. */ 
    NATIONAL_ITEM,
    /** A DBCS character string. */ 
    DBCS_ITEM,
    /** An alphanumeric string with editing characters. */ 
    ALPHANUMERIC_EDITED_ITEM,
    /** An alphanumeric string. */ 
    ALPHANUMERIC_ITEM,
    /** A string of binary data (not translated). */ 
    OCTET_STREAM_ITEM,
    /** A 4 bytes hexadecimal floating point numeric. */ 
    SINGLE_FLOAT_ITEM,
    /** An 8 bytes hexadecimal floating point numeric. */ 
    DOUBLE_FLOAT_ITEM,
    /** An packed decimal numeric. */ 
    PACKED_DECIMAL_ITEM,
    /** A string of digits also known as zoned decimal. */ 
    ZONED_DECIMAL_ITEM,
    /** A numeric string with editing characters. */ 
    NUMERIC_EDITED_ITEM,
    /** An array index. */ 
    INDEX_ITEM,
    /** An pointer to a memory location. */ 
    POINTER_ITEM ,
    /** An pointer to a procedure. */ 
    PROC_POINTER_ITEM,
    /** An pointer to a function. */ 
    FUNC_POINTER_ITEM,
    /** A reference to an object. */ 
    OBJECT_ITEM,
    /** A string of characters representing a floating point numeric. */ 
    EXTERNAL_FLOATING_ITEM,
    /** A binary item with fixed number of digits. */ 
    BINARY_ITEM,
    /** A binary item . */ 
    NATIVE_BINARY_ITEM
}
