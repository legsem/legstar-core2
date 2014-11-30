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
 * XML markup for Cobol annotations.
 *
 */
public final class CobolMarkup {

    /** Namespace for cobol annotations. */
    public static final String NS = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd";
    /** Cobol annotation for elements. */
    public static final  String ELEMENT = "cobolElement";
    /* XSD annotation tags mapped to java cobol annotations */
    /** Cobol level number. */
    public static final  String LEVEL_NUMBER = "levelNumber";
    /** Cobol variable name. */
    public static final  String COBOL_NAME = "cobolName";
    /** Cobol variable type. */
    public static final  String TYPE = "type";
    /** Cobol picture clause. */
    public static final  String PICTURE = "picture";
    /** Cobol usage clause. */
    public static final  String USAGE = "usage";
    /** Cobol value clause. */
    public static final  String VALUE = "value";
    /** Cobol right or left justification. */
    public static final  String IS_JUSTIFIED_RIGHT = "justifiedRight";
    /** Cobol numeric sign indicator. */
    public static final  String IS_SIGNED = "signed";
    /** Cobol numeric total number of digits. */
    public static final  String TOTAL_DIGITS = "totalDigits";
    /** Cobol fractional number of digits. */
    public static final  String FRACTION_DIGITS = "fractionDigits";
    /** Cobol sign position. */
    public static final  String IS_SIGN_LEADING = "signLeading";
    /** Cobol sign in its own byte. */
    public static final  String IS_SIGN_SEPARATE = "signSeparate";
    /** Cobol array minimum number of occurences. */
    public static final  String MIN_OCCURS = "minOccurs";
    /** Cobol array maximum number of occurences. */
    public static final  String MAX_OCCURS = "maxOccurs";
    /** Cobol array size dependency on another variable value. */
    public static final  String DEPENDING_ON = "dependingOn";
    /** Cobol array with variable size indicator. */
    public static final  String IS_ODO_OBJECT = "isODOObject";
    /** Cobol variable redefining another one. */
    public static final  String REDEFINES = "redefines";
    /** Cobol variable is redefined by at least one other variable. */
    public static final  String IS_REDEFINED = "isRedefined";
    /** Identifies this element as used in custom code. */
    public static final  String IS_CUSTOM_VARIABLE = "customVariable";
    /** Name of class providing logic to help with alternative selection
     * when marshaling (Java to Host). */
    public static final  String MARSHAL_CHOICE_STRATEGY =
        "marshalChoiceStrategyClassName";
    /** Name of class providing logic to help with alternative selection
     * when unmarshaling (Host to Java). */
    public static final  String UNMARSHAL_CHOICE_STRATEGY =
        "unmarshalChoiceStrategyClassName";
    /** Original source file location of the Cobol description. */
    public static final  String SRCE_LINE =  "srceLine";
    /** Cobol value appears inline in cobolElement. */
    public static final  String ELEMENT_VALUE = "value";
    /** Cobol annotation for complex types. */
    public static final  String COMPLEX_TYPE = "cobolComplexType";
    /** The java class name bound to a cobol element. */
    public static final  String JAVA_CLASS_NAME = "javaClassName";
    
    /** Utility class.*/
    private CobolMarkup() {
        
    }
}
