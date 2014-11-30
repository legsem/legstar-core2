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

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.legstar.cob2xsd.XsdDataItem.XsdType;
import com.legstar.cob2xsd.antlr.RecognizerErrorHandler;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolUsage.Usage;

import static org.junit.Assert.*;

/**
 * Test the XSD mapping of elementary COBOL data items.
 * 
 */
public class XsdDataItemTest extends AbstractTest {

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * Test XML element name derived from COBOL name.
     */
    @Test
    public void testFormatElementName() {
        List < String > nonUniqueCobolNames = new ArrayList < String >();

        assertEquals("", XsdDataItem.formatElementName(new CobolDataItem(""),
                nonUniqueCobolNames, defaultConfig, null, 0));
        assertEquals("a", XsdDataItem.formatElementName(new CobolDataItem("A"),
                nonUniqueCobolNames, defaultConfig, null, 0));
        assertEquals("ab", XsdDataItem.formatElementName(
                new CobolDataItem("AB"), nonUniqueCobolNames, defaultConfig,
                null, 0));
        assertEquals("ab9C", XsdDataItem.formatElementName(new CobolDataItem(
                "AB9C"), nonUniqueCobolNames, defaultConfig, null, 0));
        assertEquals("ab9Cd", XsdDataItem.formatElementName(new CobolDataItem(
                "AB9CD"), nonUniqueCobolNames, defaultConfig, null, 0));
        assertEquals("ab9CdE", XsdDataItem.formatElementName(new CobolDataItem(
                "AB9CD-E"), nonUniqueCobolNames, defaultConfig, null, 0));
        assertEquals("ab9CdEf", XsdDataItem.formatElementName(
                new CobolDataItem("AB9CD-EF"), nonUniqueCobolNames,
                defaultConfig, null, 0));

        configProps.put(Cob2XsdConfig.ELEMENT_NAMES_START_WITH_UPPERCASE,
                Boolean.toString(true));
        Cob2XsdConfig config = new Cob2XsdConfig(configProps);
        assertEquals("Ab9CdEf", XsdDataItem.formatElementName(
                new CobolDataItem("AB9CD-EF"), nonUniqueCobolNames, config,
                null, 0));

    }

    /**
     * Test XML complex type name derived from COBOL name.
     */
    @Test
    public void testFormatTypeName() {

        List < String > nonUniqueCobolNames = new ArrayList < String >();

        /* Test name conflict resolution (append srce line) */
        nonUniqueCobolNames.add("AB9CD-EF");
        CobolDataItem cobolDataItem = new CobolDataItem("AB9CD-EF");
        cobolDataItem.setSrceLine(18);
        assertEquals("Ab9CdEf18", XsdDataItem.formatTypeName("ab9CdEf",
                cobolDataItem, nonUniqueCobolNames, defaultConfig, null, 0));

        /* Test name conflict resolution (prepend parent type name) */
        configProps.put(Cob2XsdConfig.NAME_CONFLICT_PREPEND_PARENT_NAME,
                Boolean.toString(true));
        Cob2XsdConfig config = new Cob2XsdConfig(configProps);

        CobolDataItem cobolParent = new CobolDataItem("COBOL-PARENT");
        XsdDataItem xsdParent = new XsdDataItem(cobolParent, config, null, 0,
                nonUniqueCobolNames, _errorHandler);
        assertEquals("CobolParentAb9CdEf", XsdDataItem.formatTypeName(
                "ab9CdEf", cobolDataItem, nonUniqueCobolNames, config,
                xsdParent, 0));
    }

    /**
     * Test group items.
     */
    @Test
    public void testGroupItems() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setCobolName("COBOL-NAME");
        dataItem.getChildren().add(new CobolDataItem());
        XsdDataItem mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("GROUP_ITEM", mapper.getCobolType().toString());
        assertEquals("COMPLEX", mapper.getXsdType().toString());
        assertEquals("CobolName", mapper.getXsdTypeName());
        assertEquals("cobolName", mapper.getXsdElementName());
    }

    /**
     * See if COBOL USAGE clause is mapped correctly.
     */
    @Test
    public void testSetFromUsage() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setUsage(Usage.BINARY);
        XsdDataItem mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.SINGLEFLOAT);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("SINGLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("FLOAT", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DOUBLEFLOAT);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("DOUBLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("DOUBLE", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.INDEX);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("INDEX_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.POINTER);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PROCEDUREPOINTER);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("PROC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.FUNCTIONPOINTER);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("FUNC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY1);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIONAL);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

    }

    /**
     * Test deriving type from picture clause.
     */
    @Test
    public void testSetFromPicture() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("A");
        XsdDataItem mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ALPHABETIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("X");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("X9");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ALPHANUMERIC_EDITED_ITEM", mapper.getCobolType()
                .toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("G");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("N");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("E");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("EXTERNAL_FLOATING_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("BZ0,+-CRDB$");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setPicture("99");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("USHORT", mapper.getXsdType().toString());

        dataItem.setPicture("99.9");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

    }

    /**
     * Test setting numeric attributes from usage and picture.
     */
    @Test
    public void testSetNumericAttributes() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("99V9");
        dataItem.setUsage(Usage.DISPLAY);
        XsdDataItem mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("99V9", mapper.getPicture());

        dataItem.setPicture("99.9");
        dataItem.setUsage(Usage.DISPLAY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("99.9", mapper.getPicture());

        dataItem.setPicture("S99V9");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());

        dataItem.setPicture("S999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("SHORT", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());

        dataItem.setPicture("999999999");
        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("UINT", mapper.getXsdType().toString());
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());

        dataItem.setPicture("S99999999999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("LONG", mapper.getXsdType().toString());
        assertEquals(11, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
    }

    /**
     * Test setting numeric attributes from edited numerics picture.
     */
    @Test
    public void testSetNumericAttributeEditedNumerics() {

        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("99.9");
        dataItem.setUsage(Usage.DISPLAY);
        XsdDataItem mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("99.9", mapper.getPicture());

        configProps.put(Cob2XsdConfig.CURRENCY_SIGN, "USD");
        Cob2XsdConfig config = new Cob2XsdConfig(configProps);
        dataItem.setPicture("$$$$.99");
        mapper = new XsdDataItem(dataItem, config, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(5, mapper.getTotalDigits());
        assertEquals(2, mapper.getFractionDigits());
        assertEquals(9, mapper.getLength());
        assertEquals(9, mapper.getMaxStorageLength());

        dataItem.setPicture("+,+++,999.99");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(8, mapper.getTotalDigits());
        assertEquals(2, mapper.getFractionDigits());

        dataItem.setPicture("$B*,***,***.**BBDB");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(2, mapper.getFractionDigits());

        dataItem.setPicture("$Z,ZZZ,ZZZ.ZZCR");
        mapper = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(2, mapper.getFractionDigits());
    }

    /**
     * Test what happens when the ODOObject is not found.
     */
    @Test
    public void testUpdateDependencyNoMatch() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        CobolDataItem child1 = new CobolDataItem("DEPENDON-NAME");
        child1.setDependingOn("ODO-OBJECT-NAME");

        dataItem.getChildren().add(child1);

        new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);

    }

    /**
     * Test with ODO object in direct parent.
     */
    @Test
    public void testUpdateDependencyInParent() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        CobolDataItem child0 = new CobolDataItem("SIBLING-NAME");
        CobolDataItem child1 = new CobolDataItem("ODO-OBJECT-NAME");
        CobolDataItem child2 = new CobolDataItem("DEPENDON-NAME");
        child2.setDependingOn("ODO-OBJECT-NAME");

        dataItem.getChildren().add(child0);
        dataItem.getChildren().add(child1);
        dataItem.getChildren().add(child2);

        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, defaultConfig,
                null, 0, new ArrayList < String >(), _errorHandler);
        assertFalse(xsdDataItem.getChildren().get(0).isODOObject());
        assertTrue(xsdDataItem.getChildren().get(1).isODOObject());
        assertFalse(xsdDataItem.getChildren().get(2).isODOObject());

    }

    /**
     * Test with ODO object in ancestor.
     */
    @Test
    public void testUpdateDependencyInAncestor() {
        CobolDataItem grandParent = new CobolDataItem("GRAND-PARENT-NAME");
        CobolDataItem odo = new CobolDataItem("ODO-OBJECT-NAME");
        grandParent.getChildren().add(odo);

        CobolDataItem parent = new CobolDataItem("PARENT-NAME");
        CobolDataItem dep = new CobolDataItem("DEPENDON-NAME");
        dep.setDependingOn("ODO-OBJECT-NAME");
        parent.getChildren().add(dep);

        grandParent.getChildren().add(parent);

        XsdDataItem xsdGrandParent = new XsdDataItem(grandParent,
                defaultConfig, null, 0, new ArrayList < String >(),
                _errorHandler);

        assertTrue(xsdGrandParent.getChildren().get(0).isODOObject());

    }

    /**
     * Test with Redefined object in ancestor.
     */
    @Test
    public void testUpdateRedefinitionInAncestor() {
        CobolDataItem grandParent = new CobolDataItem("GRAND-PARENT-NAME");
        CobolDataItem redefined = new CobolDataItem("REDEFINED-OBJECT-NAME");
        grandParent.getChildren().add(redefined);

        CobolDataItem parent = new CobolDataItem("PARENT-NAME");
        CobolDataItem redefining = new CobolDataItem("REDEFINING-NAME");
        redefining.setRedefines("REDEFINED-OBJECT-NAME");
        parent.getChildren().add(redefining);

        grandParent.getChildren().add(parent);

        XsdDataItem xsdGrandParent = new XsdDataItem(grandParent,
                defaultConfig, null, 0, new ArrayList < String >(),
                _errorHandler);

        assertTrue(xsdGrandParent.getChildren().get(0).isRedefined());

    }

    /**
     * Test a RENAMES data entry.
     */
    @Test
    public void testRenames() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-RENAME");
        dataItem.setLevelNumber(66);

        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, defaultConfig,
                null, 0, new ArrayList < String >(), _errorHandler);
        assertTrue(xsdDataItem.getXsdType() == null);

    }

    /**
     * Test a CONDITION data entry.
     */
    @Test
    public void testCondition() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-CONDITION");
        dataItem.setLevelNumber(88);

        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, defaultConfig,
                null, 0, new ArrayList < String >(), _errorHandler);
        assertEquals(XsdType.ENUM, xsdDataItem.getXsdType());

    }

    /**
     * Test that storage length is evaluated correctly for elementary data
     * items.
     */
    @Test
    public void testStorageLengthElementary() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setUsage(Usage.SINGLEFLOAT);
        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, defaultConfig,
                null, 0, new ArrayList < String >(), _errorHandler);
        assertEquals(4, xsdDataItem.getMinStorageLength());
        assertEquals(4, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.DOUBLEFLOAT);
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(8, xsdDataItem.getMinStorageLength());
        assertEquals(8, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.DISPLAY);
        dataItem.setPicture("X(5)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(5, xsdDataItem.getMinStorageLength());
        assertEquals(5, xsdDataItem.getMaxStorageLength());

        dataItem.setPicture("A(5)G(2)99");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(11, xsdDataItem.getMinStorageLength());
        assertEquals(11, xsdDataItem.getMaxStorageLength());

        dataItem.setPicture("G(3)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(6, xsdDataItem.getMinStorageLength());
        assertEquals(6, xsdDataItem.getMaxStorageLength());

        dataItem.setPicture("N(5)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(10, xsdDataItem.getMinStorageLength());
        assertEquals(10, xsdDataItem.getMaxStorageLength());

        dataItem.setPicture("+++99V99$");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(8, xsdDataItem.getMinStorageLength());
        assertEquals(8, xsdDataItem.getMaxStorageLength());

        dataItem.setPicture("9(18)V99");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(20, xsdDataItem.getMinStorageLength());
        assertEquals(20, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.BINARY);
        dataItem.setPicture("9(4)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(2, xsdDataItem.getMinStorageLength());
        assertEquals(2, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.BINARY);
        dataItem.setPicture("9(8)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(4, xsdDataItem.getMinStorageLength());
        assertEquals(4, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.NATIVEBINARY);
        dataItem.setPicture("S9(18)");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(8, xsdDataItem.getMinStorageLength());
        assertEquals(8, xsdDataItem.getMaxStorageLength());

        dataItem.setUsage(Usage.PACKEDDECIMAL);
        dataItem.setPicture("S9(7)V99");
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(5, xsdDataItem.getMinStorageLength());
        assertEquals(5, xsdDataItem.getMaxStorageLength());

        dataItem.setMinOccurs(0);
        dataItem.setMaxOccurs(2);
        xsdDataItem = new XsdDataItem(dataItem, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(0, xsdDataItem.getMinStorageLength());
        assertEquals(10, xsdDataItem.getMaxStorageLength());

    }

    /**
     * Test that storage length is evaluated correctly for structure data items.
     */
    @Test
    public void testStorageLengthStructures() {
        CobolDataItem struct = new CobolDataItem("PARENT");
        CobolDataItem child0 = new CobolDataItem("CHILD-0");
        CobolDataItem child1 = new CobolDataItem("CHILD-1");
        CobolDataItem child2 = new CobolDataItem("CHILD-2");
        CobolDataItem grandChild0 = new CobolDataItem("GRAND-CHILD-0");
        CobolDataItem grandChild1 = new CobolDataItem("GRAND-CHILD-1");

        child0.setUsage(Usage.SINGLEFLOAT);
        struct.getChildren().add(child0);

        grandChild0.setUsage(Usage.SINGLEFLOAT);
        grandChild1.setUsage(Usage.SINGLEFLOAT);
        child1.getChildren().add(grandChild0);
        child1.getChildren().add(grandChild1);
        struct.getChildren().add(child1);

        child2.setUsage(Usage.SINGLEFLOAT);
        struct.getChildren().add(child2);

        XsdDataItem xsdDataItem = new XsdDataItem(struct, defaultConfig, null,
                0, new ArrayList < String >(), _errorHandler);
        assertEquals(16, xsdDataItem.getMinStorageLength());
        assertEquals(16, xsdDataItem.getMaxStorageLength());

        child1.setMinOccurs(2);
        child1.setMaxOccurs(5);

        xsdDataItem = new XsdDataItem(struct, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(24, xsdDataItem.getMinStorageLength());
        assertEquals(48, xsdDataItem.getMaxStorageLength());

        struct.setMaxOccurs(2);
        xsdDataItem = new XsdDataItem(struct, defaultConfig, null, 0,
                new ArrayList < String >(), _errorHandler);
        assertEquals(48, xsdDataItem.getMinStorageLength());
        assertEquals(96, xsdDataItem.getMaxStorageLength());
    }

    /**
     * Test that storage length is evaluated correctly for structure containing
     * redefines.
     */
    @Test
    public void testStorageLengthRedfines() {
        CobolDataItem struct = new CobolDataItem("PARENT");

        CobolDataItem child0 = new CobolDataItem("CHILD-0");
        child0.setUsage(Usage.SINGLEFLOAT);

        CobolDataItem child1 = new CobolDataItem("CHILD-1");
        child1.setUsage(Usage.DOUBLEFLOAT);
        child1.setRedefines("CHILD-0");

        CobolDataItem child2 = new CobolDataItem("CHILD-2");
        child2.setUsage(Usage.SINGLEFLOAT);
        CobolDataItem child3 = new CobolDataItem("CHILD-3");
        child3.setUsage(Usage.DOUBLEFLOAT);
        child3.setRedefines("CHILD-2");
        CobolDataItem child4 = new CobolDataItem("CHILD-4");
        child4.setUsage(Usage.SINGLEFLOAT);
        child4.setRedefines("CHILD-2");

        struct.getChildren().add(child0);
        struct.getChildren().add(child1);
        struct.getChildren().add(child2);
        struct.getChildren().add(child3);
        struct.getChildren().add(child4);

        XsdDataItem xsdDataItem = new XsdDataItem(struct, defaultConfig, null,
                0, new ArrayList < String >(), _errorHandler);
        assertEquals(16, xsdDataItem.getMinStorageLength());
        assertEquals(16, xsdDataItem.getMaxStorageLength());
    }
}
