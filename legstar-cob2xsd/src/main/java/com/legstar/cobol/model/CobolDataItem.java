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
package com.legstar.cobol.model;

import java.util.LinkedList;
import java.util.List;

import com.legstar.cobol.model.CobolUsage.Usage;


/**
 * Model of a COBOL data entry.
 * 
 */
public class CobolDataItem {

    /** Level in the hierarchy this element was parsed from. */
    private int levelNumber = 1;

    /** Cobol element name. */
    private String cobolName = "FILLER";

    /** These correspond to the 3 different formats for data entries. */
    public enum DataEntryType {
        DATA_DESCRIPTION, RENAMES, CONDITION
    };

    /** Cobol element sharing same memory location. */
    private String redefines;

    /** Blank when zero clause. */
    private boolean blankWhenZero;

    /** External clause. */
    private boolean isExternal;

    /** Global clause. */
    private boolean isGlobal;

    /** Group usage national clause. */
    private boolean groupUsageNational;

    /** String justification. */
    private boolean isJustifiedRight;

    /** Cobol picture clause. */
    private String picture;

    /** True if COBOL data item has a SIGN clause. */
    private boolean isSign;

    /** Sign clause specifies sign in leading byte or trailing byte. */
    private boolean isSignLeading;

    /**
     * Sign clause specifies if sign clause specifies sign in separate byte
     * (overpunch).
     */
    private boolean isSignSeparate;

    /** Item must be synchronized on natural boundary in storage. */
    private boolean isSynchronized;

    /**
     * Arrays minimum number of occurrences. A value of -1 means this was not
     * explicitly set.
     */
    private int minOccurs = -1;

    /** Arrays maximum number of occurrences. */
    private int maxOccurs = -1;

    /** Cobol element giving array actual size. */
    private String dependingOn;

    /** Cobol indexed by sub-clauses. */
    private List < String > indexes = new LinkedList < String >();

    /** Cobol ascending key is sub-clauses. */
    private List < String > ascendingKeys = new LinkedList < String >();

    /** Cobol descending key is sub-clauses. */
    private List < String > descendingKeys = new LinkedList < String >();

    /** Cobol usage. */
    private Usage usage = null;

    /** Cobol value clause. */
    private String value;

    /** Cobol date format clause. */
    private String dateFormat;

    /** Line number in the original source file this element was parsed from. */
    private int srceLine;

    /** Ordered list of direct children. */
    private List < CobolDataItem > children = new LinkedList < CobolDataItem >();

    /** Used with RENAMES clause when there is a single subject. */
    private String renamesSubject;

    /** Used with RENAMES clause when there is a range of subjects. */
    private Range renamesSubjectRange;

    /** Used with condition clauses for one or more literal values. */
    private List < String > conditionLiterals = new LinkedList < String >();

    /** Used with condition clauses for one or more ranges of literal values. */
    private List < Range > conditionRanges = new LinkedList < Range >();

    /**
     * No argument constructor.
     */
    public CobolDataItem() {

    }

    /**
     * Constructor used when the level number will be determined at a later
     * time.
     * 
     * @param cobolName the data item COBOL name
     */
    public CobolDataItem(final String cobolName) {
        this.cobolName = cobolName;
    }

    /**
     * Constructor used when the level number is known.
     * 
     * @param levelNumber the data item COBOL name
     * @param cobolName the data item COBOL name
     */
    public CobolDataItem(final int levelNumber, final String cobolName) {
        this.levelNumber = levelNumber;
        this.cobolName = cobolName;
    }

    /**
     * @return the Level in the hierarchy
     */
    public int getLevelNumber() {
        return levelNumber;
    }

    /**
     * @param levelNumber the Level in the hierarchy to set
     */
    public void setLevelNumber(final int levelNumber) {
        this.levelNumber = levelNumber;
    }

    /**
     * @return the Cobol element name
     */
    public String getCobolName() {
        return cobolName;
    }

    /**
     * @param cobolName the Cobol element name to set
     */
    public void setCobolName(final String cobolName) {
        this.cobolName = cobolName;
    }

    /**
     * @return the Cobol element sharing same memory location
     */
    public String getRedefines() {
        return redefines;
    }

    /**
     * @param redefines Cobol element sharing same memory location to set
     */
    public void setRedefines(final String redefines) {
        this.redefines = redefines;
    }

    /**
     * @return the blank when zero clause
     */
    public boolean isBlankWhenZero() {
        return blankWhenZero;
    }

    /**
     * @param blankWhenZero the blank when zero clause to set
     */
    public void setBlankWhenZero(final boolean blankWhenZero) {
        this.blankWhenZero = blankWhenZero;
    }

    /**
     * @return the external clause
     */
    public boolean isExternal() {
        return isExternal;
    }

    /**
     * @param external the external clause to set
     */
    public void setExternal(final boolean external) {
        this.isExternal = external;
    }

    /**
     * @return the global clause
     */
    public boolean isGlobal() {
        return isGlobal;
    }

    /**
     * @param global the global clause to set
     */
    public void setGlobal(final boolean global) {
        this.isGlobal = global;
    }

    /**
     * @return the group usage national clause
     */
    public boolean isGroupUsageNational() {
        return groupUsageNational;
    }

    /**
     * @param groupUsageNational the group usage national clause to set
     */
    public void setGroupUsageNational(final boolean groupUsageNational) {
        this.groupUsageNational = groupUsageNational;
    }

    /**
     * @return true if String is right justified
     */
    public boolean isJustifiedRight() {
        return isJustifiedRight;
    }

    /**
     * @param justifiedRight true if String is right justified
     */
    public void setJustifiedRight(final boolean justifiedRight) {
        this.isJustifiedRight = justifiedRight;
    }

    /**
     * @return the Cobol picture clause
     */
    public String getPicture() {
        return picture;
    }

    /**
     * @param picture the Cobol picture clause to set
     */
    public void setPicture(final String picture) {
        this.picture = picture;
    }

    /**
     * @return true if COBOL data item has a SIGN clause
     */
    public boolean isSign() {
        return isSign;
    }

    /**
     * @param isSign true if COBOL data item has a SIGN clause
     */
    public void setSign(final boolean isSign) {
        this.isSign = isSign;
    }

    /**
     * @return true if sign clause specifies sign in leading byte (false means
     *         trailing byte)
     */
    public boolean isSignLeading() {
        return isSignLeading;
    }

    /**
     * @param isSignLeading true itrue if sign clause specifies sign in leading
     *            byte (false means trailing byte)
     */
    public void setSignLeading(final boolean isSignLeading) {
        setSign(true);
        this.isSignLeading = isSignLeading;
    }

    /**
     * @return true if sign clause specifies sign in separate byte (overpunch)
     */
    public boolean isSignSeparate() {
        return isSignSeparate;
    }

    /**
     * @param isSignSeparate true if sign clause specifies sign in separate byte
     *            (overpunch)
     */
    public void setSignSeparate(final boolean isSignSeparate) {
        setSign(true);
        this.isSignSeparate = isSignSeparate;
    }

    /**
     * @return true if the item must be synchronized on natural boundary in
     *         storage
     */
    public boolean isSynchronized() {
        return isSynchronized;
    }

    /**
     * @param isSynchronized true if the item must be synchronized on natural
     *            boundary in storage
     */
    public void setSynchronized(final boolean isSynchronized) {
        this.isSynchronized = isSynchronized;
    }

    /**
     * @return the minimum number of occurrences
     */
    public int getMinOccurs() {
        return minOccurs;
    }

    /**
     * @param minOccurs the minimum number of occurrences to set
     */
    public void setMinOccurs(final int minOccurs) {
        this.minOccurs = minOccurs;
    }

    /**
     * @return the maximum number of occurrences
     */
    public int getMaxOccurs() {
        return maxOccurs;
    }

    /**
     * @param maxOccurs the maximum number of occurrences to set
     */
    public void setMaxOccurs(final int maxOccurs) {
        this.maxOccurs = maxOccurs;
    }

    /**
     * @return the Cobol element giving array actual size
     */
    public String getDependingOn() {
        return dependingOn;
    }

    /**
     * @param dependingOn the Cobol element giving array actual size to set
     */
    public void setDependingOn(final String dependingOn) {
        this.dependingOn = dependingOn;
    }

    /**
     * @return the cobol indexed by sub-clauses
     */
    public List < String > getIndexes() {
        return indexes;
    }

    /**
     * @param indexes the cobol indexed by sub-clauses to set
     */
    public void setIndexes(final List < String > indexes) {
        this.indexes = indexes;
    }

    /**
     * @param index a cobol index to add
     */
    public void addIndex(final String index) {
        indexes.add(index);
    }

    /**
     * @return the cobol ascending key is sub-clauses
     */
    public List < String > getAscendingKeys() {
        return ascendingKeys;
    }

    /**
     * @param ascendingKeys the cobol ascending key is sub-clauses to set
     */
    public void setAscendingKeys(final List < String > ascendingKeys) {
        this.ascendingKeys = ascendingKeys;
    }

    /**
     * @param ascendingKey a cobol ascending key to add
     */
    public void addAscendingKey(final String ascendingKey) {
        ascendingKeys.add(ascendingKey);
    }

    /**
     * @return the cobol descending key is sub-clauses
     */
    public List < String > getDescendingKeys() {
        return descendingKeys;
    }

    /**
     * @param descendingKeys the cobol descending key is sub-clauses to set
     */
    public void setDescendingKeys(final List < String > descendingKeys) {
        this.descendingKeys = descendingKeys;
    }

    /**
     * @param descendingKey a cobol descending key to add
     */
    public void addDescendingKey(final String descendingKey) {
        descendingKeys.add(descendingKey);
    }

    /**
     * @return the Cobol usage clause (enum)
     */
    public Usage getUsage() {
        return usage;
    }

    /**
     * @param usage the Cobol usage to set
     */
    public void setUsage(final Usage usage) {
        this.usage = usage;
    }

    /**
     * @return the Cobol usage clause (as a COBOL string)
     */
    public String getCobolUsage() {
        return CobolUsage.getCobolUsage(usage);
    }

    /**
     * Set the Usage using the COBOL using string.
     * 
     * @param cobolUsage the COBOL usage string
     */
    public void setCobolUsage(String cobolUsage) {
        this.usage = CobolUsage.getUsage(cobolUsage);
    }

    /**
     * Value is a COBOL value clause. For string literals it must include start
     * and end delimiters. Delimiters must be escaped in a COBOL acceptable way.
     * 
     * @return the Cobol value clause
     */
    public String getValue() {
        return value;
    }

    /**
     * Value is a COBOL value clause. For string literals it must include start
     * and end delimiters. Delimiters must be escaped in a COBOL acceptable way.
     * 
     * @param value the Cobol values to set
     */
    public void setValue(final String value) {
        this.value = value;
    }

    /**
     * @return the cobol date format clause
     */
    public String getDateFormat() {
        return dateFormat;
    }

    /**
     * @param dateFormat the cobol date format clause to set
     */
    public void setDateFormat(final String dateFormat) {
        this.dateFormat = dateFormat;
    }

    /**
     * @return the Line number in the original source file
     */
    public int getSrceLine() {
        return srceLine;
    }

    /**
     * @param srceLine the Line number in the original source file to set
     */
    public void setSrceLine(final int srceLine) {
        this.srceLine = srceLine;
    }

    /**
     * @return the ordered list of direct children
     */
    public List < CobolDataItem > getChildren() {
        return children;
    }

    /**
     * @param children the ordered list of direct children to set
     */
    public void setChildren(final List < CobolDataItem > children) {
        this.children = children;
    }

    /**
     * @return true if this data item is a structure (group).
     */
    public boolean isStructure() {
        return (getChildren().size() > 0);
    }

    /**
     * @return the single subject of a RENAMES clause
     */
    public String getRenamesSubject() {
        return renamesSubject;
    }

    /**
     * @param renamesSubject the single subject of a RENAMES clause to set
     */
    public void setRenamesSubject(final String renamesSubject) {
        this.renamesSubject = renamesSubject;
    }

    /**
     * @return the range of subjects of a RENAMES clause
     */
    public Range getRenamesSubjectRange() {
        return renamesSubjectRange;
    }

    /**
     * @param renamesSubjectRange the range of subjects of a RENAMES clause to
     *            set
     */
    public void setRenamesSubjectRange(final Range renamesSubjectRange) {
        this.renamesSubjectRange = renamesSubjectRange;
    }

    /**
     * @return the one or more literal values of a condition
     */
    public List < String > getConditionLiterals() {
        return conditionLiterals;
    }

    /**
     * @param conditionLiterals the one or more literal values of a condition to
     *            set
     */
    public void setConditionLiterals(final List < String > conditionLiterals) {
        this.conditionLiterals = conditionLiterals;
    }

    /**
     * @param conditionLiteral a literal value of a condition to set
     */
    public void addConditionLiterals(final String conditionLiteral) {
        conditionLiterals.add(conditionLiteral);
    }

    /**
     * @return the one or more ranges of literal values of a condition
     */
    public List < Range > getConditionRanges() {
        return conditionRanges;
    }

    /**
     * @param conditionRanges the one or more ranges of literal values of a
     *            condition to set
     */
    public void setConditionRanges(final List < Range > conditionRanges) {
        this.conditionRanges = conditionRanges;
    }

    /**
     * @param conditionRange the range of literal values of a condition to set
     */
    public void addConditionRange(final Range conditionRange) {
        conditionRanges.add(conditionRange);
    }

    /**
     * Represents a range between two literals.
     */
    public static class Range {
        /** Range start. */
        private String _from;
        /** Range stop. */
        private String _to;

        /**
         * Constructor for immutable class.
         * 
         * @param from range start
         * @param to range stop
         */
        public Range(final String from, final String to) {
            _from = from;
            _to = to;
        }

        /**
         * @return the range start
         */
        public String getFrom() {
            return _from;
        }

        /**
         * @return the range stop
         */
        public String getTo() {
            return _to;
        }

        /** {@inheritDoc} */
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append('{');
            sb.append("from:" + getFrom());
            sb.append(',');
            sb.append("to:" + getTo());
            sb.append('}');
            return sb.toString();
        }
    }

    /**
     * @return the data entry type inferred from the level
     */
    public DataEntryType getDataEntryType() {
        return (levelNumber == 88) ? DataEntryType.CONDITION
                : (levelNumber == 66) ? DataEntryType.RENAMES
                        : DataEntryType.DATA_DESCRIPTION;
    }

    /**
     * @return true if this is a condition (level 88)
     */
    public boolean isCondition() {
        return getDataEntryType().equals(DataEntryType.CONDITION);
    }

    /**
     * @return true if this is a RENAMES clause (level 66)
     */
    public boolean isRenames() {
        return getDataEntryType().equals(DataEntryType.RENAMES);
    }

    /**
     * @return true if this is a regular data description entry (not a condition
     *         or nemae clause)
     */
    public boolean isDataDescription() {
        return getDataEntryType().equals(DataEntryType.DATA_DESCRIPTION);
    }

    /**
     * in COBOL arrays are either occurring items (normal arrays) or optional
     * items (they occur no time or once).
     * 
     * @return true if this item is an array
     */
    public boolean isArray() {
        return (getMaxOccurs() > 1 || (getMaxOccurs() == 1 && getMinOccurs() == 0));
    }

    /**
     * @return true if this is a variable size array
     */
    public boolean isVariableSizeArray() {
        return isArray()
                && getMinOccurs() != getMaxOccurs()
                && (getDependingOn() != null && getDependingOn().trim()
                        .length() > 0);
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        sb.append("level:" + getLevelNumber());
        sb.append(',');
        sb.append("cobolName:" + getCobolName());

        if (getDataEntryType() != null) {
            switch (getDataEntryType()) {
            case DATA_DESCRIPTION:
                toStringDataDescription(sb);
                break;
            case RENAMES:
                toStringRenames(sb);
                break;
            case CONDITION:
                toStringCondition(sb);
                break;
            default:
                break;
            }
        }
        sb.append(',');
        sb.append("srceLine:" + getSrceLine());
        sb.append('}');
        return sb.toString();
    }

    /**
     * Pretty printing for a complete data description entry.
     * 
     * @param sb the string builder
     */
    private void toStringDataDescription(final StringBuilder sb) {
        if (getRedefines() != null) {
            sb.append(',');
            sb.append("redefines:" + getRedefines());
        }
        if (isBlankWhenZero()) {
            sb.append(',');
            sb.append("isBlankWhenZero:" + isBlankWhenZero());
        }
        if (isExternal()) {
            sb.append(',');
            sb.append("isExternal:" + isExternal());
        }
        if (isGlobal()) {
            sb.append(',');
            sb.append("isGlobal:" + isGlobal());
        }
        if (isGroupUsageNational()) {
            sb.append(',');
            sb.append("groupUsageNational:" + isGroupUsageNational());
        }
        if (getMaxOccurs() > 0) {
            if (getMinOccurs() > -1) {
                sb.append(',');
                sb.append("minOccurs:" + getMinOccurs());
            }
            sb.append(',');
            sb.append("maxOccurs:" + getMaxOccurs());
            if (getDependingOn() != null) {
                sb.append(',');
                sb.append("dependingOn:" + getDependingOn());
            }
            if (getIndexes().size() > 0) {
                toStringList(sb, getIndexes(), "indexes");
            }
            if (getAscendingKeys().size() > 0) {
                toStringList(sb, getAscendingKeys(), "ascendingKeys");
            }
            if (getDescendingKeys().size() > 0) {
                toStringList(sb, getDescendingKeys(), "descendingKeys");
            }
        }
        if (isSign()) {
            sb.append(',');
            sb.append("isSign:" + isSign());
            sb.append(',');
            sb.append("isSignLeading:" + isSignLeading());
            sb.append(',');
            sb.append("isSignSeparate:" + isSignSeparate());
        }
        if (isSynchronized()) {
            sb.append(',');
            sb.append("isSynchronized:" + isSynchronized());
        }
        if (getUsage() != null) {
            sb.append(',');
            sb.append("usage:" + getUsage());
        }
        if (getChildren().size() == 0) {
            if (isJustifiedRight()) {
                sb.append(',');
                sb.append("isJustifiedRight:" + isJustifiedRight());
            }
            if (getPicture() != null) {
                sb.append(',');
                sb.append("picture:" + '\"' + getPicture() + '\"');
            }
        }
        if (getValue() != null && getValue().length() > 0) {
            sb.append(',');
            sb.append("value:" + getValue());
        }
        if (getDateFormat() != null) {
            sb.append(',');
            sb.append("dateFormat:" + getDateFormat());
        }
        if (getChildren().size() > 0) {
            toStringList(sb, getChildren(), "children");
        }
    }

    /**
     * Pretty printing for a renames entry.
     * 
     * @param sb the string builder
     */
    private void toStringRenames(final StringBuilder sb) {
        if (getRenamesSubject() != null) {
            sb.append(',');
            sb.append("renamesSubject:" + getRenamesSubject());
        }
        if (getRenamesSubjectRange() != null) {
            sb.append(',');
            sb.append("renamesSubjectRange:" + getRenamesSubjectRange());
        }

    }

    /**
     * Pretty printing for a condition entry.
     * 
     * @param sb the string builder
     */
    private void toStringCondition(final StringBuilder sb) {
        if (getConditionLiterals().size() > 0) {
            toStringList(sb, getConditionLiterals(), "conditionLiterals");
        }
        if (getConditionRanges().size() > 0) {
            toStringList(sb, getConditionRanges(), "conditionRanges");
        }

    }

    /**
     * Adds list elements to a string builder.
     * 
     * @param sb the string builder
     * @param list list of elements
     * @param title name of elements list
     */
    private void toStringList(final StringBuilder sb, final List < ? > list,
            final String title) {
        sb.append(',');
        sb.append(title + ":[");
        boolean first = true;
        for (Object child : list) {
            if (!first) {
                sb.append(",");
            } else {
                first = false;
            }
            sb.append(child.toString());
        }
        sb.append("]");
    }

}
