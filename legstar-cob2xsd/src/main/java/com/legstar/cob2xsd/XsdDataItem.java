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

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cob2xsd.antlr.RecognizerErrorHandler;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.DataEntryType;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.cobol.model.CobolTypes;
import com.legstar.cobol.model.CobolUsage.Usage;
import com.legstar.cobol.utils.PictureUtil;

/**
 * XML Schema attributes derived from a COBOL data item.
 * <p>
 * Acts as a facade to the {@link CobolDataItem} type. This class is constructed
 * from a CobolDataItem. All XML Schema attributes are derived at construction
 * time.
 * <p>
 * The isODOObject and isRedefined properties are the only ones that are not set
 * at construction time (And therefore have setters). This is because these
 * members can be set only when some data item downstream happens to reference
 * this one.
 */
public class XsdDataItem {

    /** The COBOL data item this facade is built from. */
    private CobolDataItem _cobolDataItem;

    /** XSD simple built-in types. */
    public enum XsdType {
        /** Maps XML Schema types. */
        COMPLEX, ENUM, STRING, HEXBINARY, SHORT, USHORT, INT, UINT, LONG, ULONG, INTEGER, DECIMAL, FLOAT, DOUBLE
    };

    /** The parent data item or null if root. */
    private XsdDataItem _parent;

    /** Ordered list of direct children. */
    private List < XsdDataItem > _children = new LinkedList < XsdDataItem >();

    /** XML schema type mapping the COBOL data item. */
    private XsdType _xsdType;

    /** XML Schema element name. */
    private String _xsdElementName;

    /** Complex types are named (as opposed to anonymous). */
    private String _xsdTypeName;

    /** A derived COBOL type used in annotations. */
    private CobolTypes _cobolType;

    /** For xsd:string and xsd:hexBinary. */
    private int _length = -1;

    /** For xsd:string derived from numeric edited. */
    private String _pattern;

    /** For xsd numeric types, the total number of digits. */
    private int _totalDigits = -1;

    /** For xsd numeric types, the fractional digits. */
    private int _fractionDigits = -1;

    /**
     * Determines if a numeric item is signed or unsigned. This is different
     * from the COBOL data item _isSign member which denotes the presence of the
     * special SIGN COBOL clause (position of the sign as leading or trailing).
     */
    private boolean _isSigned;

    /*
     * Array boundaries have a different semantic in XSD. MinOccurs defaults to
     * MaxOccurs in COBOL while, it defaults to 1 in XSD. The meaning of
     * maxOccurs = 1 is also different: it is meant as an array of 1 dimension
     * in COBOL while it means "not an array" in XSD.
     */
    /** Arrays minimum number of occurrences. */
    private int _minOccurs = 1;

    /** Arrays maximum number of occurrences. */
    private int _maxOccurs = 1;

    /** True if some array size downstream depends on this data item value. */
    private boolean _isODOObject;

    /** True if some item downstream redefines this data item. */
    private boolean _isRedefined;

    /** The name of a upstream element that this element redefines. */
    private String _redefines;

    /** Minimum number of storage bytes occupied by this item in z/OS memory. */
    private int _minStorageLength;

    /** Maximum number of storage bytes occupied by this item in z/OS memory. */
    private int _maxStorageLength;

    /** A prefix to use when a COBOL name starts with an illegal XML character. */
    public static final String SAFE_NAME_PREFIX = "C";

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler;

    /** Logger. */
    private static final Logger _log = LoggerFactory
            .getLogger(XsdDataItem.class);

    /**
     * COBOL data item is analyzed at construction time and all XSD attributes
     * are derived from the COBOL attributes.
     * 
     * @param cobolDataItem the COBOL elementary data item
     * @param config the translator options in effect
     * @param parent the parent data item or null if root
     * @param order order within parent to disambiguate siblings
     * @param nonUniqueCobolNames a list of non unique COBOL names used to
     *            detect name collisions
     * @param errorHandler collects translation errors
     */
    public XsdDataItem(final CobolDataItem cobolDataItem,
            final Cob2XsdConfig config, final XsdDataItem parent,
            final int order, final List < String > nonUniqueCobolNames,
            final RecognizerErrorHandler errorHandler) {

        _errorHandler = errorHandler;
        _cobolDataItem = cobolDataItem;
        _parent = parent;
        _xsdElementName = formatElementName(cobolDataItem, nonUniqueCobolNames,
                config, parent, order);
        _xsdTypeName = formatTypeName(_xsdElementName, cobolDataItem,
                nonUniqueCobolNames, config, parent, order);

        switch (cobolDataItem.getDataEntryType()) {
        case DATA_DESCRIPTION:
            setDataDescription(cobolDataItem, config, nonUniqueCobolNames);
            break;
        case RENAMES:
            /* COBOL renames don't map to an XSD type. */
            addMessageToHistory(
                    "Unhandled data entry type " + cobolDataItem.toString(),
                    "warn");
            break;
        case CONDITION:
            /* Will map to an enumeration facet. */
            _xsdType = XsdType.ENUM;
            if (config.mapConditionsToFacets()
                    && getConditionRanges().size() > 1) {
                addMessageToHistory(
                        "Condition with multiple ranges cannot be mapped to enumeration facet "
                                + cobolDataItem.toString(), "warn");
            }
            break;
        default:
            addMessageToHistory(
                    "Unrecognized data entry type " + cobolDataItem.toString(),
                    "error");
        }

    }

    /**
     * Setup a regular data description entry, elementary data items and groups.
     * 
     * @param cobolDataItem the COBOL elementary data item
     * @param config the translator options in effect
     * @param nonUniqueCobolNames a list of non unique COBOL names used to
     *            detect name collisions
     */
    protected void setDataDescription(final CobolDataItem cobolDataItem,
            final Cob2XsdConfig config,
            final List < String > nonUniqueCobolNames) {

        /* Group items have non level 88 children */
        if (isGroup(cobolDataItem)) {
            _xsdType = XsdType.COMPLEX;
            _cobolType = CobolTypes.GROUP_ITEM;
        } else {

            if (cobolDataItem.getUsage() != null) {
                setAttributesFromUsage(cobolDataItem.getUsage());
            } else {
                /* Check parent for any usage we should inherit from */
                if (getParent() != null && getParent().getUsage() != null) {
                    setAttributesFromUsage(getParent().getUsage());
                }
            }

            if (cobolDataItem.getPicture() != null) {
                setAttributesFromPicture(cobolDataItem.getPicture(),
                        cobolDataItem.isSignSeparate(),
                        cobolDataItem.isBlankWhenZero(),
                        config.getCurrencySign(), config.getCurrencySymbol()
                                .charAt(0), config.nSymbolDbcs(),
                        config.decimalPointIsComma());
            }
        }
        _maxStorageLength = _minStorageLength;

        /*
         * If the xsdType is not set yet, then this is likely some syntax we
         * don't support.
         */
        if (_xsdType == null) {
            addMessageToHistory(
                    "Unable to determine type for " + cobolDataItem.toString()
                            + ". Assuming group item.", "warn");
            _xsdType = XsdType.COMPLEX;
            _cobolType = CobolTypes.GROUP_ITEM;
        }

        /* Inform ODO object upstream that an array size depends on him. */
        if (getDependingOn() != null && getParent() != null) {
            boolean odoObjectMarked = markODOObjectInAncestor(getDependingOn());
            if (!odoObjectMarked) {
                addMessageToHistory(
                        "Unable to locate ODO object named " + getDependingOn()
                                + " for item " + cobolDataItem.toString(),
                        "warn");
            }
        }

        /* Inform object upstream that someone redefines him. */
        _redefines = cobolDataItem.getRedefines();
        if (_redefines != null && getParent() != null) {
        	_redefines = getParent().updateRedefinition(_redefines);
        }

        /* Create the list of children by decorating the COBOL item children. */
        int order = 0;
        for (CobolDataItem child : cobolDataItem.getChildren()) {
            XsdDataItem xsdChild = new XsdDataItem(child, config, this, order,
                    nonUniqueCobolNames, _errorHandler);
            _children.add(xsdChild);
            order++;
        }

        /*
         * Children contribute their storage length to their parent unless they
         * are part of a redefine group in which case, the largest child in the
         * group is the one contributing)
         */
        boolean redefines = false;
        int minRedefinesStorageLength = 0;
        int maxRedefinesStorageLength = 0;
        for (XsdDataItem xsdChild : getChildren()) {
            if (redefines) {
                if (xsdChild.getRedefines() == null) {
                    redefines = false;
                    _minStorageLength += minRedefinesStorageLength;
                    _maxStorageLength += maxRedefinesStorageLength;
                } else {
                    if (xsdChild.getMinStorageLength() > minRedefinesStorageLength) {
                        minRedefinesStorageLength = xsdChild
                                .getMinStorageLength();
                    }
                    if (xsdChild.getMaxStorageLength() > maxRedefinesStorageLength) {
                        maxRedefinesStorageLength = xsdChild
                                .getMaxStorageLength();
                    }
                }
            }
            if (xsdChild.isRedefined()) {
                redefines = true;
                minRedefinesStorageLength = xsdChild.getMinStorageLength();
                maxRedefinesStorageLength = xsdChild.getMaxStorageLength();
            } else {
                if (!redefines) {
                    _minStorageLength += xsdChild.getMinStorageLength();
                    _maxStorageLength += xsdChild.getMaxStorageLength();
                }
            }
        }
        if (redefines) {
            _minStorageLength += minRedefinesStorageLength;
            _maxStorageLength += maxRedefinesStorageLength;
        }

        /*
         * Set XSD minOccurs/maxOccurs from COBOL. If no minOccurs set in COBOL,
         * this is a fixed size array (unless a depending on clause exists).
         */
        if (cobolDataItem.getMaxOccurs() > 0) {
            _maxOccurs = cobolDataItem.getMaxOccurs();
            _maxStorageLength = _maxStorageLength * _maxOccurs;
            if (cobolDataItem.getDependingOn() == null) {
                if (cobolDataItem.getMinOccurs() > -1) {
                    _minOccurs = cobolDataItem.getMinOccurs();
                } else {
                    _minOccurs = cobolDataItem.getMaxOccurs();
                }
            } else {
                _minOccurs = 0;
            }
             _minStorageLength = _minStorageLength * _minOccurs;
        }

    }

    /**
     * Items are identified as group items if they has at least one child that
     * is not a condition.
     * <p>
     * If a group item has a picture clause assume a COBOL syntax error issue a
     * warning and ignore children.
     * 
     * @param cobolDataItem the COBOL data item to check for groupness
     * @return true if this is a group item
     */
    protected boolean isGroup(final CobolDataItem cobolDataItem) {
        boolean isGroup = false;
        if (cobolDataItem.getChildren() != null) {
            for (CobolDataItem child : cobolDataItem.getChildren()) {
                if (child.getDataEntryType() != DataEntryType.CONDITION) {
                    isGroup = true;
                }
            }
        }
        if (isGroup && cobolDataItem.getPicture() != null) {
            addMessageToHistory("Group item with picture clause "
                    + cobolDataItem.toString()
                    + ". Assuming elementary item and children are ignored",
                    "warn");
            isGroup = false;
        }
        return isGroup;

    }

    /**
     * Lookup our ancestors for the ODO object of an array.
     * <p>
     * First we ask our direct parent to lookup in his children (stopping when
     * he reaches us).
     * <p>
     * If we can't find in the immediate parent, then we move up to the grand
     * parent.
     * 
     * @param odoObjectCobolName the depending on object name.
     * @return true when the ODO object was found and marked
     */
    public boolean markODOObjectInAncestor(final String odoObjectCobolName) {
        boolean found = false;
        if (getParent() == null) {
            return found;
        }
        found = getParent().markODOObjectInChildren(odoObjectCobolName,
                getCobolName());
        if (found) {
            return found;
        }
        return getParent().markODOObjectInAncestor(odoObjectCobolName);
    }

    /**
     * Lookup an ODO object in this item's children.
     * <p>
     * Recurses through the grand children until found or no more children to
     * lookup.
     * 
     * @param odoObjectCobolName the ODO Object we are looking for
     * @param stopChildCobolName a child name past which it is not useful to
     *            continue the lookup
     * @return
     */
    public boolean markODOObjectInChildren(final String odoObjectCobolName,
            final String stopChildCobolName) {
        boolean found = false;
        for (XsdDataItem child : getChildren()) {
            if (child.getCobolName().equals(stopChildCobolName)) {
                break;
            }
            if (child.getCobolName().equals(odoObjectCobolName)) {
                child.setIsODOObject(true);
                found = true;
                break;
            }
            found = child.markODOObjectInChildren(odoObjectCobolName,
                    stopChildCobolName);
            if (found) {
                break;
            }
        }
        return found;
    }

	/**
	 * Called when some child (or child of a child) has a REDEFINES clause. We
	 * look up our children for an item matching the COBOL name of the REDEFINES
	 * object. If found, we update its isRedefined member, otherwise we
	 * propagate the request to our own parent.
	 * <p>
	 * In case the redefined parent is itself redefining a grand parent
	 * (transitive redefines), we don't update the isRedefined property and send
	 * back the grand parent name (to be used as the true redefines).
	 * 
	 * @param cobolName
	 *            the redefines object.
	 * @return the ultimate redefined element name (may differ from cobolName in
	 *         case of transitive dependencies)
	 */
	public String updateRedefinition(final String cobolName) {
		for (XsdDataItem child : getChildren()) {
			if (child.getCobolName().equals(cobolName)) {
				if (child.getRedefines() != null) {
					return child.getRedefines();
				}
				child.setIsRedefined(true);
				return cobolName;
			}
		}
		if (getParent() != null) {
			return getParent().updateRedefinition(cobolName);
		}
		_log.error("Unable to locate redefined item " + cobolName);
		return null;
	}

    /**
     * Derive XML schema attributes from a COBOL usage clause. This gives a
     * rough approximation of the XSD type because the picture clause usually
     * carries info that needs to be further analyzed to determine a more
     * precise type. If no usage clause, we assume there will be a picture
     * clause.
     * 
     * @param usage COBOL usage clause
     */
    private void setAttributesFromUsage(final Usage usage) {
        switch (usage) {
        case BINARY:
            _cobolType = CobolTypes.BINARY_ITEM;
            _xsdType = XsdType.INTEGER;
            break;
        case NATIVEBINARY:
            _cobolType = CobolTypes.NATIVE_BINARY_ITEM;
            _xsdType = XsdType.INTEGER;
            break;
        case SINGLEFLOAT:
            _cobolType = CobolTypes.SINGLE_FLOAT_ITEM;
            _xsdType = XsdType.FLOAT;
            _minStorageLength = 4;
            break;
        case DOUBLEFLOAT:
            _cobolType = CobolTypes.DOUBLE_FLOAT_ITEM;
            _xsdType = XsdType.DOUBLE;
            _minStorageLength = 8;
            break;
        case PACKEDDECIMAL:
            _cobolType = CobolTypes.PACKED_DECIMAL_ITEM;
            _xsdType = XsdType.DECIMAL;
            break;
        case INDEX:
            _cobolType = CobolTypes.INDEX_ITEM;
            _xsdType = XsdType.HEXBINARY;
            _minStorageLength = 4;
            break;
        case POINTER:
            _cobolType = CobolTypes.POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            _minStorageLength = 4;
            break;
        case PROCEDUREPOINTER:
            _cobolType = CobolTypes.PROC_POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            _minStorageLength = 8;
            break;
        case FUNCTIONPOINTER:
            _cobolType = CobolTypes.FUNC_POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            _minStorageLength = 4;
            break;
        case DISPLAY:
            _cobolType = CobolTypes.ALPHANUMERIC_ITEM;
            _xsdType = XsdType.STRING;
            break;
        case DISPLAY1:
            _cobolType = CobolTypes.DBCS_ITEM;
            _xsdType = XsdType.STRING;
            break;
        case NATIONAL:
            _cobolType = CobolTypes.NATIONAL_ITEM;
            _xsdType = XsdType.STRING;
            break;
        default:
            _log.error("Unrecognized usage clause " + toString());
        }
    }

    /**
     * Derive XML schema attributes from a COBOL picture.
     * 
     * @param picture the picture clause
     * @param isSignSeparate if sign occupies a separated position (no
     *            overpunch)
     * @param isBlankWhenZero item contains only spaces when its value is zero
     * @param currencySign the currency sign
     * @param currencySymbol the currency symbol
     * @param nSymbolDbcs true if COBOL compiler option NSYMBOL(DBCS)
     * @param decimalPointIsComma if COBOL compiler option DECIMAL POINT IS
     *            COMMA
     */
    private void setAttributesFromPicture(final String picture,
            final boolean isSignSeparate, final boolean isBlankWhenZero,
            final String currencySign, final char currencySymbol,
            final boolean nSymbolDbcs, final boolean decimalPointIsComma) {

        char comma = (decimalPointIsComma) ? '.' : ',';
        char decimalPoint = (decimalPointIsComma) ? ',' : '.';

        Map < Character, Integer > charNum = PictureUtil
                .getPictureCharOccurences(picture, currencySymbol);

        _length = PictureUtil.calcLengthFromPicture(charNum, isSignSeparate,
                currencySign, currencySymbol, false);
        /*
         * storage is valid only for simple strings at this stage. will be
         * refined later.
         */
        _pattern = PictureUtil.getRegexFromPicture(picture, currencySign,
                currencySymbol);

        if ((charNum.get('A') + charNum.get('X')) > 0) {
            if ((charNum.get('9') + charNum.get('B') + charNum.get('0') + charNum
                    .get('/')) > 0) {
                _cobolType = CobolTypes.ALPHANUMERIC_EDITED_ITEM;
            } else {
                if (charNum.get('X') == 0) {
                    _cobolType = CobolTypes.ALPHABETIC_ITEM;
                } else {
                    _cobolType = CobolTypes.ALPHANUMERIC_ITEM;
                }
            }
            _xsdType = XsdType.STRING;
            _minStorageLength = PictureUtil.calcLengthFromPicture(charNum,
                    isSignSeparate, currencySign, currencySymbol, true);
            return;
        }

        if (charNum.get('G') > 0) {
            _cobolType = CobolTypes.DBCS_ITEM;
            _xsdType = XsdType.STRING;
            _minStorageLength = PictureUtil.calcLengthFromPicture(charNum,
                    isSignSeparate, currencySign, currencySymbol, true);
            return;
        }

        if (charNum.get('N') > 0) {
            if (nSymbolDbcs) {
                _cobolType = CobolTypes.DBCS_ITEM;
            } else {
                _cobolType = CobolTypes.NATIONAL_ITEM;
            }
            _xsdType = XsdType.STRING;
            _minStorageLength = PictureUtil.calcLengthFromPicture(charNum,
                    isSignSeparate, currencySign, currencySymbol, true);
            return;
        }

        /* TODO Was previously mapped to xsd:float but requires more analysis */
        if (charNum.get('E') > 0) {
            _cobolType = CobolTypes.EXTERNAL_FLOATING_ITEM;
            _xsdType = XsdType.STRING;
            _minStorageLength = _length;
            return;
        }

        /*
         * Numeric edited items are identified by their picture clause symbols
         * or the presence of the BLANK WHEN ZERO clause
         */
        if (((charNum.get('/') + charNum.get('B') + charNum.get('/')
                + charNum.get('Z') + charNum.get('0') + charNum.get(comma)
                + charNum.get(decimalPoint) + charNum.get('*')
                + charNum.get('+') + charNum.get('-') + charNum.get('C') /* CR */
                + charNum.get('D') /* DB */
        + charNum.get(currencySymbol)) > 0)
                || isBlankWhenZero) {
            _cobolType = CobolTypes.NUMERIC_EDITED_ITEM;
            _xsdType = XsdType.STRING;
            _minStorageLength = _length;
            /* Adding digits and sign for numeric edited is experimental. */
            setDigitsAndSign(picture, currencySymbol, decimalPointIsComma);
            return;
        }

        /* At this stage we are left with pure numeric picture clauses. */

        /*
         * If usage was DISPLAY, we can now refine since we now know it is a
         * numeric not an alphanumeric.
         */
        if (_cobolType == null || _cobolType == CobolTypes.ALPHANUMERIC_ITEM) {
            _cobolType = CobolTypes.ZONED_DECIMAL_ITEM;
            _minStorageLength = _length;
        }
        setNumericAttributes(picture, currencySymbol, decimalPointIsComma);

    }

    /**
     * Once we have identified the COBOL data item as being numeric, this will
     * perform more analysis on the picture clause to extract such info as
     * integer part, decimal part and sign.
     * <p>
     * The fractionDigits corresponds to digits past the decimal point. The
     * totalDigits is the integer part + fractionDigits;
     * <p>
     * Once digits are identified we can further refine the choice of XML schema
     * type and a set of associated facets.
     * 
     * @param picture a purely numeric picture clause
     * @param currencyChar the currency sign
     * @param decimalPointIsComma true if decimal point is comma
     */
    private void setNumericAttributes(final String picture,
            final char currencyChar, final boolean decimalPointIsComma) {

        setDigitsAndSign(picture, currencyChar, decimalPointIsComma);

        if (_fractionDigits == 0) {
            if (_totalDigits < 5) {
                _xsdType = (_isSigned) ? XsdType.SHORT : XsdType.USHORT;
            } else if (_totalDigits < 10) {
                _xsdType = (_isSigned) ? XsdType.INT : XsdType.UINT;
            } else if (_totalDigits < 20) {
                _xsdType = (_isSigned) ? XsdType.LONG : XsdType.ULONG;
            } else {
                _xsdType = XsdType.INTEGER;
            }

        } else {
            _xsdType = XsdType.DECIMAL;
        }

        switch (_cobolType) {
        case BINARY_ITEM:
        case NATIVE_BINARY_ITEM:
            if (_totalDigits < 5) {
                _minStorageLength = 2;
            } else if (_totalDigits < 10) {
                _minStorageLength = 4;
            } else {
                _minStorageLength = 8;
            }
            break;
        case PACKED_DECIMAL_ITEM:
            _minStorageLength = (_totalDigits / 2) + 1;
            break;
        default:
            break;
        }

    }

    /**
     * Extracts total number of digits, fraction digits and sign from a picture
     * clause.
     * <p>
     * Works for zoned decimals, binary and packed decimal.
     * <p>
     * 
     * @param picture a purely numeric picture clause
     * @param currencySymbol the currency symbol
     * @param decimalPointIsComma true if decimal point is comma
     */
    protected void setDigitsAndSign(final String picture,
            final char currencySymbol, final boolean decimalPointIsComma) {

        char decimalPoint = (decimalPointIsComma) ? ',' : '.';

        /*
         * Look for the integer part (digits before the decimal point) Decimal
         * point is virtual or not.
         */
        int iV = picture.indexOf('V');
        if (iV == -1) {
            iV = picture.indexOf('v');
        }
        if (iV == -1) {
            iV = picture.indexOf(decimalPoint);
        }
        Map < Character, Integer > intCharNum;
        Map < Character, Integer > decCharNum;
        if (iV > 0) {
            intCharNum = PictureUtil.getPictureCharOccurences(
                    picture.substring(0, iV), currencySymbol);
            decCharNum = PictureUtil.getPictureCharOccurences(
                    picture.substring(iV), currencySymbol);
            _fractionDigits = getMaxDigits(decCharNum, currencySymbol);
            _totalDigits = getMaxDigits(intCharNum, currencySymbol)
                    + _fractionDigits;
        } else {
            intCharNum = PictureUtil.getPictureCharOccurences(picture,
                    currencySymbol);
            _fractionDigits = 0;
            _totalDigits = getMaxDigits(intCharNum, currencySymbol);
        }

        _isSigned = ((intCharNum.get('S') + intCharNum.get('+') + intCharNum
                .get('-')) > 0) ? true : false;

    }

    /**
     * The maximum number of digits supported by a numeric is given by its
     * picture clause.
     * <p>
     * Currency symbol, + and - can be used for floating insertion editing in
     * which case they are repeated more than once.
     * 
     * @param intCharNum the picture symbols map
     * @param currencySymbol the currency symbol in use
     * @return the maximum number of digits supported
     */
    protected int getMaxDigits(final Map < Character, Integer > intCharNum,
            final char currencySymbol) {
        int maxDigits = intCharNum.get('9') + intCharNum.get('Z')
                + intCharNum.get('*');
        if (intCharNum.get('+') > 0) {
            maxDigits += intCharNum.get('+') - 1;
        }
        if (intCharNum.get('-') > 0) {
            maxDigits += intCharNum.get('-') - 1;
        }
        if (intCharNum.get(currencySymbol) > 0) {
            maxDigits += intCharNum.get(currencySymbol) - 1;
        }
        return maxDigits;
    }

    /**
     * Turn a COBOL name to an XSD element name.
     * <p>
     * COBOL names look ugly in XML schema. They are often uppercased and use
     * hyphens extensively. XML schema names they will have to be transformed
     * later to java identifiers so we try to get as close as possible to a
     * naming convention that suits XML Schema as well as Java.
     * <p>
     * So we remove hyphens. We lower case all characters which are not word
     * breakers. Word breakers are hyphens and numerics. This creates Camel
     * style names. Element names customarily start with a lowercase character.
     * <p>
     * COBOL FILLERs are a particular case because there might be more than one
     * in the same parent group. So what we do is systematically append the
     * COBOL source line number so that these become unique names.
     * <p>
     * COBOL names can start with a digit which is illegal for XML element
     * names. In this case we prepend a "C" character.
     * <p>
     * Since Enterprise COBOL V4R1, underscores can be used (apart from first
     * character). We treat them as hyphens here, they are not propagated to the
     * XSD name but are used as word breakers.
     * <p>
     * Once an element name is identified, we make sure it is unique among
     * siblings within the same parent.
     * 
     * @param cobolDataItem the original COBOL data item
     * @param nonUniqueCobolNames a list of non unique COBOL names used to
     *            detect name collisions
     * @param config the translator options
     * @param parent used to resolve potential name conflict
     * @param order order within parent to disambiguate siblings
     * @return an XML schema element name
     */
    public static String formatElementName(final CobolDataItem cobolDataItem,
            final List < String > nonUniqueCobolNames,
            final Cob2XsdConfig config, final XsdDataItem parent,
            final int order) {

        String cobolName = getXmlCompatibleCobolName(cobolDataItem
                .getCobolName());
        if (cobolName.equalsIgnoreCase("FILLER")) {
            String filler = (config.elementNamesStartWithUppercase()) ? "Filler"
                    : "filler";
            return filler + cobolDataItem.getSrceLine();
        }

        StringBuilder sb = new StringBuilder();
        boolean wordBreaker = (config.elementNamesStartWithUppercase()) ? true
                : false;
        for (int i = 0; i < cobolName.length(); i++) {
            char c = cobolName.charAt(i);
            if (c != '-' && c != '_') {
                if (Character.isDigit(c)) {
                    sb.append(c);
                    wordBreaker = true;
                } else {
                    if (wordBreaker) {
                        sb.append(Character.toUpperCase(c));
                    } else {
                        sb.append(Character.toLowerCase(c));
                    }
                    wordBreaker = false;
                }
            } else {
                wordBreaker = true;
            }
        }

        String elementName = sb.toString();
        if (parent != null) {
            int siblingsWithSameName = 0;
            for (CobolDataItem child : parent.getCobolChildren()) {
                if (child.getCobolName().equals(cobolDataItem.getCobolName())) {
                    siblingsWithSameName++;
                }
            }
            if (siblingsWithSameName > 1) {
                elementName += order;
            }
        }

        return elementName;
    }

    /**
     * Turn a COBOL name to a unique XSD type name.
     * <p>
     * Complex type names customarily start with an uppercase character.
     * <p>
     * The proposed name might be conflicting with another so we disambiguate
     * xsd type names with one of 2 options:
     * <ul>
     * <li>Appending the COBOL source line number (compatible with
     * legstar-schemagen)</li>
     * <li>Appending the parent XSD type name</li>
     * </ul>
     * 
     * @param cobolDataItem the COBOL data item
     * @param elementName the element name built from the COBOL name
     * @param nonUniqueCobolNames a list of non unique COBOL names
     * @param config the translator options
     * @param parent used to resolve potential name conflict
     * @param order order within parent to disambiguate siblings
     * @return a nice XML type name
     */
    public static String formatTypeName(final String elementName,
            final CobolDataItem cobolDataItem,
            final List < String > nonUniqueCobolNames,
            final Cob2XsdConfig config, final XsdDataItem parent,
            final int order) {

        StringBuilder sb = new StringBuilder();
        sb.append(Character.toUpperCase(elementName.charAt(0)));
        sb.append(elementName.substring(1));
        if (nonUniqueCobolNames.contains(cobolDataItem.getCobolName())) {
            if (config.nameConflictPrependParentName()) {
                if (parent != null) {
                    sb.insert(0, parent.getXsdTypeName());
                }
            } else {
                sb.append(cobolDataItem.getSrceLine());
            }
        }

        return sb.toString();
    }

    /**
     * Transform the COBOL name to a valid XML Name. Does not do any
     * beautification other than strictly complying with XML specifications.
     * 
     * @param cobolName the original COBOL data item name
     * @return a valid XML Name
     */
    public static String getXmlCompatibleCobolName(final String cobolName) {
        if (cobolName != null && cobolName.length() > 0
                && Character.isDigit(cobolName.charAt(0))) {
            return SAFE_NAME_PREFIX + cobolName;
        } else {
            return cobolName;
        }
    }

    /**
     * @return the XML schema type mapping the COBOL data item
     */
    public XsdType getXsdType() {
        return _xsdType;
    }

    /**
     * @return the A derived COBOL type used in annotations
     */
    public CobolTypes getCobolType() {
        return _cobolType;
    }

    /**
     * @return the For xsd:string and xsd:hexBinary
     */
    public int getLength() {
        return _length;
    }

    /**
     * @return the For xsd:string derived from numeric edited
     */
    public String getPattern() {
        return _pattern;
    }

    /**
     * @return the For xsd numeric types, the total number of digits
     */
    public int getTotalDigits() {
        return _totalDigits;
    }

    /**
     * @return the For xsd numeric types, the fractional digits
     */
    public int getFractionDigits() {
        return _fractionDigits;
    }

    /**
     * @return the XML Schema element name
     */
    public String getXsdElementName() {
        return _xsdElementName;
    }

    /**
     * @return the XML Schema type name
     */
    public String getXsdTypeName() {
        return _xsdTypeName;
    }

    /**
     * @return the ordered list of direct children
     */
    public List < XsdDataItem > getChildren() {
        return _children;
    }

    /**
     * @return the minimum number of occurrences (XSD semantic)
     */
    public int getMinOccurs() {
        return _minOccurs;
    }

    /**
     * @return the minimum number of occurrences (COBOL semantic)
     */
    public int getCobolMinOccurs() {
        return _cobolDataItem.getMinOccurs();
    }

    /**
     * @return the maximum number of occurrences (XSD semantic)
     */
    public int getMaxOccurs() {
        return _maxOccurs;
    }

    /**
     * @return the maximum number of occurrences (COBOL semantic)
     */
    public int getCobolMaxOccurs() {
        return _cobolDataItem.getMaxOccurs();
    }

    /**
     * @return the Level in the hierarchy
     */
    public int getLevelNumber() {
        return _cobolDataItem.getLevelNumber();
    }

    /**
     * @return the Cobol element name
     */
    public String getCobolName() {
        return _cobolDataItem.getCobolName();
    }

    /**
     * @return the Cobol picture clause
     */
    public String getPicture() {
        return _cobolDataItem.getPicture();
    }

    /**
     * @return the Cobol usage clause (enum)
     */
    public Usage getUsage() {
        return _cobolDataItem.getUsage();
    }

    /**
     * @return the Cobol generic usage. This is needed because COBOL usage
     *         values are not accepted as java identifiers.
     */
    public String getUsageForCobol() {
        switch (getUsage()) {
        case BINARY:
            return "BINARY";
        case SINGLEFLOAT:
            return "COMP-1";
        case DOUBLEFLOAT:
            return "COMP-2";
        case PACKEDDECIMAL:
            return "PACKED-DECIMAL";
        case NATIVEBINARY:
            return "COMP-5";
        case DISPLAY:
            return "DISPLAY";
        case DISPLAY1:
            return "DISPLAY-1";
        case INDEX:
            return "INDEX";
        case NATIONAL:
            return "NATIONAL";
        case POINTER:
            return "POINTER";
        case PROCEDUREPOINTER:
            return "PROCEDURE-POINTER";
            /** Function pointer. */
        case FUNCTIONPOINTER:
            return "FUNCTION-POINTER";
        default:
            return null;
        }
    }

    /**
     * @return true if String is right justified
     */
    public boolean isJustifiedRight() {
        return _cobolDataItem.isJustifiedRight();
    }

    /**
     * @return true if COBOL data item has a SIGN clause
     */
    public boolean isSign() {
        return _cobolDataItem.isSign();
    }

    /**
     * @return true if sign clause specifies sign in leading byte (false means
     *         trailing byte)
     */
    public boolean isSignLeading() {
        return _cobolDataItem.isSignLeading();
    }

    /**
     * @return true if sign clause specifies sign in separate byte (overpunch)
     */
    public boolean isSignSeparate() {
        return _cobolDataItem.isSignSeparate();
    }

    /**
     * @return the Cobol element giving array actual size
     */
    public String getDependingOn() {
        return _cobolDataItem.getDependingOn();
    }

    /**
     * @return true if a numeric item is signed
     */
    public boolean isSigned() {
        return _isSigned;
    }

    /**
     * @return the Cobol element sharing same memory location
     */
    public String getRedefines() {
        return _redefines;
    }

    /**
     * @return the Cobol value clause
     */
    public String getValue() {
        return _cobolDataItem.getValue();
    }

    /**
     * @return the Line number in the original source file
     */
    public int getSrceLine() {
        return _cobolDataItem.getSrceLine();
    }

    /**
     * @return the data entry type. Could also be inferred from the level
     */
    public DataEntryType getDataEntryType() {
        return _cobolDataItem.getDataEntryType();
    }

    /**
     * @return the one or more literal values of a condition
     */
    public List < String > getConditionLiterals() {
        return _cobolDataItem.getConditionLiterals();
    }

    /**
     * @return the one or more ranges of literal values of a condition
     */
    public List < Range > getConditionRanges() {
        return _cobolDataItem.getConditionRanges();
    }

    /**
     * @return the blank when zero clause
     */
    public boolean isBlankWhenZero() {
        return _cobolDataItem.isBlankWhenZero();
    }

    /**
     * @return the parent data item or null if root
     */
    public XsdDataItem getParent() {
        return _parent;
    }

    /**
     * @return true if some array size downstream depends on this data item
     *         value
     */
    public boolean isODOObject() {
        return _isODOObject;
    }

    /**
     * @return true if some item downstream redefines this data item
     */
    public boolean isRedefined() {
        return _isRedefined;
    }

    /**
     * @param isODOObject true if some array size downstream depends on this
     *            data item value
     */
    public void setIsODOObject(final boolean isODOObject) {
        _isODOObject = isODOObject;
    }

    /**
     * @param isRedefined true if some item downstream redefines this data item
     */
    public void setIsRedefined(final boolean isRedefined) {
        _isRedefined = isRedefined;
    }

    /**
     * @return the minimum number of storage bytes occupied by this item in z/OS
     *         memory
     */
    public int getMinStorageLength() {
        return _minStorageLength;
    }

    /**
     * @return the maximumm number of storage bytes occupied by this item in
     *         z/OS memory
     */
    public int getMaxStorageLength() {
        return _maxStorageLength;
    }

    /**
     * @return true if this is a numeric element
     */
    public boolean isNumeric() {
        switch (getXsdType()) {
        case SHORT:
        case USHORT:
        case INT:
        case UINT:
        case LONG:
        case ULONG:
        case INTEGER:
        case DECIMAL:
        case FLOAT:
        case DOUBLE:
            return true;
        default:
            return false;
        }
    }

    /**
     * @return the ordered list of direct COBOL children
     */
    public List < CobolDataItem > getCobolChildren() {
        return _cobolDataItem.getChildren();
    }

    /**
     * @param msg the last error message recorded
     * @param level a simple level to trace the error
     */
    private void addMessageToHistory(final String msg, final String level) {
        if (level.equalsIgnoreCase("warn")) {
            _log.warn(msg);
        } else if (level.equalsIgnoreCase("error")) {
            _log.error(msg);
        } else {
            _log.info(msg);
        }
        _errorHandler.addMessageToHistory(msg);
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        sb.append("typeName:" + getXsdTypeName());
        sb.append(',');
        sb.append("elementName:" + getXsdElementName());
        if (getXsdType() != null) {
            sb.append(',');
            sb.append("type:" + getXsdType().toString());
            if (getCobolType() != null) {
                sb.append(',');
                sb.append("cobolType:" + getCobolType().toString());
            }
            if (getXsdType() == XsdType.STRING
                    || getXsdType() == XsdType.HEXBINARY) {
                if (getLength() > -1) {
                    sb.append(',');
                    sb.append("length:" + getLength());
                }
                if (getPattern() != null) {
                    sb.append(',');
                    sb.append("pattern:" + getPattern());
                }
            }
            if (isNumeric()) {
                if (getTotalDigits() > -1) {
                    sb.append(',');
                    sb.append("totalDigits:" + getTotalDigits());
                }
                if (getFractionDigits() > -1) {
                    sb.append(',');
                    sb.append("fractionDigits:" + getFractionDigits());
                }
                sb.append(',');
                sb.append("isSigned:" + isSigned());
            }
            sb.append(',');
            sb.append("minOccurs:" + getMinOccurs());
            sb.append(',');
            sb.append("maxOccurs:" + getMaxOccurs());
            sb.append(',');
            sb.append("isODOObject:" + isODOObject());
            sb.append(',');
            sb.append("isRedefined:" + isRedefined());
            sb.append(',');
            sb.append("minStorageLength:" + getMinStorageLength());
            sb.append(',');
            sb.append("maxStorageLength:" + getMaxStorageLength());
        }
        sb.append(',');
        sb.append(_cobolDataItem.toString());
        return sb.toString();

    }

}
