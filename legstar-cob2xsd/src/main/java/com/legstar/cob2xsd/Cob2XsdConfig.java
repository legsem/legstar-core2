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
package com.legstar.cob2xsd;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * This class gathers execution parameters for the COBOL to XML Schema utility.
 * <p/>
 * The class is immutable.
 * 
 */
public class Cob2XsdConfig {

    /** Default configuration. */
    private static final String DEFAULT_CONFIG_RESOURCE = "/cob2xsd.properties";

    /**
     * Source can be in fixed format (sequence numbers, indicator area, area A,
     * area B) or free format.
     */
    public enum CodeFormat {
        /**
         * Fixed is the legacy format, free, the more recent one.
         */
        FIXED_FORMAT, FREE_FORMAT
    };

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

    /**
     * Default code format.
     */
    public static final String DEFAULT_CODE_FORMAT = "FIXED_FORMAT";

    /**
     * Default column where fixed format COBOL code starts (inclusive, based 1).
     */
    public static final String DEFAULT_START_COLUMN = "7";

    /** Default column where fixed format COBOL code ends (inclusive, based 1). */
    public static final String DEFAULT_END_COLUMN = "72";

    /** The default character set used to encode the XML Schema. */
    public static final String DEFAULT_XSD_ENCODING = "UTF-8";

    /** Default Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    public static final String DEFAULT_CURRENCY_SIGN = "$";

    /**
     * Default Currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     */
    public static final String DEFAULT_CURRENCY_SYMBOL = DEFAULT_CURRENCY_SIGN;

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Fixed or Free format COBOL source. */
    public static final String CODE_FORMAT = "codeFormat";

    /** For fixed format COBOL, position of the indicator area. */
    public static final String START_COLUMN = "startColumn";

    /** For fixed format COBOL position of the right margin. */
    public static final String END_COLUMN = "endColumn";

    /** Character set used to encode the output XML Schema. */
    public static final String XSD_ENCODING = "xsdEncoding";

    /** Whether COBOL conditions (level 88) should be mapped to facets. */
    public static final String MAP_CONDITIONS_TO_FACETS = "mapConditionsToFacets";

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict.
     */
    public static final String NAME_CONFLICT_PREPEND_PARENT_NAME = "nameConflictPrependParentName";

    /** True if XSD element names should start with an uppercase. */
    public static final String ELEMENT_NAMES_START_WITH_UPPERCASE = "elementNamesStartWithUppercase";

    /** True if we should ignore primitive data items without a parent group. */
    public static final String IGNORE_ORPHAN_PRIMITIVE_ELEMENTS = "ignoreOrphanPrimitiveElements";

    /** An optional XSLT transform for XML schema customization. */
    public static final String CUSTOM_XSLT_FILENAME = "customXsltFileName";

    /** Whether we should generate COBOL/JAXB annotations. */
    public static final String ADD_LEGSTAR_ANNOTATIONS = "addLegStarAnnotations";

    /** Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    public static final String CURRENCY_SIGN = "currencySign";

    /** Currency symbol used (CURRENCY PICTURE SYMBOL clause). */
    public static final String CURRENCY_SYMBOL = "currencySymbol";

    /** Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause). */
    public static final String DECIMAL_POINT_IS_COMMA = "decimalPointIsComma";

    /** COBOL NSYMBOL(DBCS) compiler option. */
    public static final String NSYMBOL_DBCS = "nSymbolDbcs";

    /** COBOL QUOTE|APOST compiler option. */
    public static final String QUOTE_IS_QUOTE = "quoteIsQuote";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /*
     * ----------------------------------------------------------------------
     * COBOL source format related options
     */

    /** Fixed or Free format COBOL source. */
    private final CodeFormat _codeFormat;

    /** For fixed format COBOL, position of the indicator area. */
    private final int _startColumn;

    /** For fixed format COBOL, position of the right margin. */
    private final int _endColumn;

    /*
     * ----------------------------------------------------------------------
     * XML Schema related options
     */

    /** Character set used to encode the output XML Schema. */
    private final String _xsdEncoding;

    /**
     * Whether COBOL conditions (level 88) should be mapped to facets. Facets
     * restrict the content which might not be desirable.
     */
    private final boolean _mapConditionsToFacets;

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict (otherwise, the COBOL source line will be appended).
     */
    private final boolean _nameConflictPrependParentName;

    /**
     * True if XSD element names should start with an uppercase (compatible with
     * LegStar 1.2).
     */
    private final boolean _elementNamesStartWithUppercase;

    /** An optional XSLT transform for XML schema customization. */
    private final String _customXsltFileName;

    /**
     * Ignore primitive data items which are not attached to a parent group.
     */
    private final boolean _ignoreOrphanPrimitiveElements;

    /*
     * ----------------------------------------------------------------------
     * LegStar annotations related options
     */

    /** Whether we should generate COBOL annotations. */
    private final boolean _addLegStarAnnotations;

    /*
     * ----------------------------------------------------------------------
     * COBOL compiler related options
     */

    /** Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    private final String _currencySign;

    /**
     * Currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     */
    private final String _currencySymbol;

    /**
     * Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the
     * SPECIAL-NAMES).
     */
    private final boolean _decimalPointIsComma;

    /** COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false. */
    private final boolean _nSymbolDbcs;

    /** COBOL QUOTE|APOST compiler option. False means APOST. */
    private final boolean _quoteIsQuote;

    /**
     * Construct configuration from a properties object.
     * 
     * @param configProps the properties object
     */
    public Cob2XsdConfig(final Properties configProps) {
        
        Properties props = configProps == null ? getDefaultConfigProps() : configProps;
        
        _codeFormat = CodeFormat.valueOf(props.getProperty(CODE_FORMAT,
                DEFAULT_CODE_FORMAT));
        _startColumn = Integer.parseInt(props.getProperty(START_COLUMN,
                DEFAULT_START_COLUMN));
        _endColumn = Integer.parseInt(props.getProperty(END_COLUMN,
                DEFAULT_END_COLUMN));

        _xsdEncoding = props.getProperty(XSD_ENCODING, DEFAULT_XSD_ENCODING);
        _mapConditionsToFacets = Boolean.parseBoolean(props.getProperty(
                MAP_CONDITIONS_TO_FACETS, "false"));
        _nameConflictPrependParentName = Boolean.parseBoolean(props
                .getProperty(NAME_CONFLICT_PREPEND_PARENT_NAME, "false"));
        _elementNamesStartWithUppercase = Boolean.parseBoolean(props
                .getProperty(ELEMENT_NAMES_START_WITH_UPPERCASE, "false"));
        _ignoreOrphanPrimitiveElements = Boolean.parseBoolean(props
                .getProperty(IGNORE_ORPHAN_PRIMITIVE_ELEMENTS, "true"));
        _customXsltFileName = props.getProperty(CUSTOM_XSLT_FILENAME);
        _addLegStarAnnotations = Boolean.parseBoolean(props.getProperty(
                ADD_LEGSTAR_ANNOTATIONS, "true"));
        _currencySign = props.getProperty(CURRENCY_SIGN, DEFAULT_CURRENCY_SIGN);
        _currencySymbol = props.getProperty(CURRENCY_SYMBOL,
                DEFAULT_CURRENCY_SYMBOL);
        _decimalPointIsComma = Boolean.parseBoolean(props.getProperty(
                DECIMAL_POINT_IS_COMMA, "false"));
        _nSymbolDbcs = Boolean.parseBoolean(props.getProperty(NSYMBOL_DBCS,
                "false"));
        _quoteIsQuote = Boolean.parseBoolean(props.getProperty(QUOTE_IS_QUOTE,
                "true"));

    }

    /**
     * Construct from a properties file.
     * 
     * @param configFile the configuration file
     */
    public Cob2XsdConfig(final File configFile) throws IOException {
        this(getConfigProps(configFile));
    }

    private static Properties getConfigProps(File configFile) throws IOException {
        return (configFile == null || !configFile.exists()) ? getDefaultConfigProps()
                : getConfigProps(new FileInputStream(configFile));
    }

    public static Properties getDefaultConfigProps() {
        try {
            return getConfigProps(Cob2XsdConfig.class
                    .getResourceAsStream(DEFAULT_CONFIG_RESOURCE));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    private static Properties getConfigProps(InputStream stream)
            throws IOException {
        try {
            Properties config = new Properties();
            config.load(stream);
            return config;
        } finally {
            stream.close();
        }
    }

    /*
     * ------------------------------------------------------------------- COBOL
     * source format related options
     */
    /**
     * @return the Fixed or Free format COBOL source
     */
    public CodeFormat getCodeFormat() {
        return _codeFormat;
    }

    /**
     * @return the position of the indicator area for fixed format COBOL
     */
    public int getStartColumn() {
        return _startColumn;
    }

    /**
     * @return the position of the right margin for fixed format COBOL
     */
    public int getEndColumn() {
        return _endColumn;
    }

    /*
     * ------------------------------------------------------------------- XML
     * Schema related options
     */

    /**
     * @return the character set used to encode the output XML Schema
     */
    public String getXsdEncoding() {
        return _xsdEncoding;
    }

    /**
     * Whether COBOL conditions (level 88) should be mapped to facets. Facets
     * restrict the content which might not be desirable.
     * 
     * @return whether COBOL conditions (level 88) should be mapped to facets.
     */
    public boolean mapConditionsToFacets() {
        return _mapConditionsToFacets;
    }

    /**
     * An optional XSLT transform for XML schema customization.
     * 
     * @return an optional XSLT transform for XML schema customization
     */
    public String getCustomXsltFileName() {
        return _customXsltFileName;
    }

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict (otherwise, the COBOL source line will be appended).
     * 
     * @return true if parent complex type name should be prepended in case of
     *         name conflict (otherwise, the COBOL source line will be appended)
     */
    public boolean nameConflictPrependParentName() {
        return _nameConflictPrependParentName;
    }

    /**
     * True if XSD element names should start with an uppercase (compatible with
     * legstar-schemagen).
     * 
     * @return true if XSD element names should start with an uppercase
     */
    public boolean elementNamesStartWithUppercase() {
        return _elementNamesStartWithUppercase;
    }

    /**
     * Ignore primitive data items which are not attached to a parent group.
     * 
     * @return true if primitive data items without a parent group are ignored
     */
    public boolean ignoreOrphanPrimitiveElements() {
        return _ignoreOrphanPrimitiveElements;
    }

    /*
     * -------------------------------------------------------------------
     * LegStar annotations related options
     */

    /**
     * Whether we should generate LegStar COBOL/JAXB annotations.
     * 
     * @return whether we should generate LegStar COBOL/JAXB annotations
     */
    public boolean addLegStarAnnotations() {
        return _addLegStarAnnotations;
    }

    /*
     * ------------------------------------------------------------------- COBOL
     * compiler related options
     */

    /**
     * The COBOL currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES).
     * 
     * @return the COBOL currency sign used
     */
    public String getCurrencySign() {
        return _currencySign;
    }

    /**
     * The COBOL currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     * 
     * @return the COBOL currency symbol used
     */
    public String getCurrencySymbol() {
        return _currencySymbol;
    }

    /**
     * Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the
     * SPECIAL-NAMES).
     * 
     * @return whether comma is the decimal point
     */
    public boolean decimalPointIsComma() {
        return _decimalPointIsComma;
    }

    /**
     * The COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if
     * false
     * 
     * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if
     *         false
     */
    public boolean nSymbolDbcs() {
        return _nSymbolDbcs;
    }

    /**
     * The COBOL QUOTE|APOST compiler option. False means APOST.
     * 
     * @return the COBOL QUOTE|APOST compiler option. False means APOST
     */
    public boolean quoteIsQuote() {
        return _quoteIsQuote;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("Cob2XsdConfig [_codeFormat=");
        builder.append(_codeFormat);
        builder.append(", _startColumn=");
        builder.append(_startColumn);
        builder.append(", _endColumn=");
        builder.append(_endColumn);
        builder.append(", _xsdEncoding=");
        builder.append(_xsdEncoding);
        builder.append(", _mapConditionsToFacets=");
        builder.append(_mapConditionsToFacets);
        builder.append(", _nameConflictPrependParentName=");
        builder.append(_nameConflictPrependParentName);
        builder.append(", _elementNamesStartWithUppercase=");
        builder.append(_elementNamesStartWithUppercase);
        builder.append(", _customXsltFileName=");
        builder.append(_customXsltFileName);
        builder.append(", _ignoreOrphanPrimitiveElements=");
        builder.append(_ignoreOrphanPrimitiveElements);
        builder.append(", _addLegStarAnnotations=");
        builder.append(_addLegStarAnnotations);
        builder.append(", _currencySign=");
        builder.append(_currencySign);
        builder.append(", _currencySymbol=");
        builder.append(_currencySymbol);
        builder.append(", _decimalPointIsComma=");
        builder.append(_decimalPointIsComma);
        builder.append(", _nSymbolDbcs=");
        builder.append(_nSymbolDbcs);
        builder.append(", _quoteIsQuote=");
        builder.append(_quoteIsQuote);
        builder.append("]");
        return builder.toString();
    }

}
