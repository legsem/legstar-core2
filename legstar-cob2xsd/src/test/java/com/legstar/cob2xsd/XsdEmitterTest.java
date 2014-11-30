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

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.cobol.model.CobolUsage.Usage;

/**
 * Test the CobolDataItemToXSD class.
 * 
 */
public class XsdEmitterTest extends AbstractXsdEmitterTester {

    /**
     * Empty COBOL item.
     */
    @Test
    public void testAnEmptyCobolItem() {
        emitAndCheck("<complexType name=\"Filler0\"><sequence/></complexType>",
                new CobolDataItem());
    }

    /**
     * A structure containing a single string item.
     */
    @Test
    public void testString() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("A-STRING", "X(5)", Usage.DISPLAY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"aString\">" + "<simpleType>"
                + "<restriction base=\"string\">" + "<maxLength value=\"5\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct);
    }

    /**
     * A short numeric items.
     */
    @Test
    public void testShort() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "S9(4)", Usage.BINARY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"short\">" + "<totalDigits value=\"4\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct);
    }

    /**
     * An unsignedShort numeric item. There should be no
     * minInclusive/maxInclusive because its native binary.
     */
    @Test
    public void testUnsignedShort() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "9(2)", Usage.NATIVEBINARY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"unsignedShort\">" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * An int numeric item.
     */
    @Test
    public void testInt() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "S9(9)", Usage.BINARY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"int\">" + "<totalDigits value=\"9\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct);
    }

    /**
     * An unsigned int numeric item.
     */
    @Test
    public void testUnsignedInt() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "9(9)", Usage.DISPLAY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"unsignedInt\">"
                + "<totalDigits value=\"9\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * A long numeric item.
     */
    @Test
    public void testLong() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "S9(18)", Usage.BINARY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"long\">" + "<totalDigits value=\"18\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct);
    }

    /**
     * An unsigned long numeric item.
     */
    @Test
    public void testUnsignedLong() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "9(12)", Usage.BINARY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"unsignedLong\">"
                + "<maxInclusive value=\"999999999999\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * A very long numeric item (ARITH(EXTEND) compiler option).
     */
    @Test
    public void testInteger() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", "S9(31)", Usage.DISPLAY));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"integer\">"
                + "<totalDigits value=\"31\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * A decimal numeric item.
     */
    @Test
    public void testDecimal() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren()
                .add(getACobolElementaryItem("NUM1", "S9(8)V99",
                        Usage.PACKEDDECIMAL));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"decimal\">"
                + "<totalDigits value=\"10\"/>"
                + "<fractionDigits value=\"2\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * An unsigned decimal numeric item.
     */
    @Test
    public void testUnsignedDecimal() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren()
                .add(getACobolElementaryItem("NUM1", "9(8)V99",
                        Usage.PACKEDDECIMAL));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"decimal\">"
                + "<totalDigits value=\"10\"/>"
                + "<fractionDigits value=\"2\"/>"
                + "<minInclusive value=\"0\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * A float numeric item.
     */
    @Test
    public void testFloat() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", null, Usage.SINGLEFLOAT));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"float\">" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * A double numeric item.
     */
    @Test
    public void testDouble() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("NUM1", null, Usage.DOUBLEFLOAT));
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"num1\">" + "<simpleType>"
                + "<restriction base=\"double\">" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct);
    }

    /**
     * An array.
     */
    @Test
    public void testArray() {
        CobolDataItem struct = new CobolDataItem();
        CobolDataItem array = getACobolElementaryItem("A-STRING", "A(5)99",
                Usage.DISPLAY);
        array.setMinOccurs(0);
        array.setMaxOccurs(3);
        struct.getChildren().add(array);
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element maxOccurs=\"3\" minOccurs=\"0\" name=\"aString\">"
                + "<simpleType>" + "<restriction base=\"string\">"
                + "<maxLength value=\"7\"/>"
                + "<pattern value=\"[\\p{L}\\s]{0,5}\\d{0,2}\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct);
    }

    /**
     * Test generation of LegStar annotations.
     */
    @Test
    public void testAnnotations() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(
                getACobolElementaryItem("A-STRING", "X(5)", Usage.DISPLAY));
        emitAndCheck(
                ""
                        + "<complexType name=\"Filler0\">"
                        + "<sequence>"
                        + "<element name=\"aString\">"
                        + "<annotation>"
                        + "<appinfo>"
                        + "<cb:cobolElement cobolName=\"A-STRING\" levelNumber=\"1\""
                        + " picture=\"X(5)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"
                        + "</appinfo>" + "</annotation>" + "<simpleType>"
                        + "<restriction base=\"string\">"
                        + "<maxLength value=\"5\"/>" + "</restriction>"
                        + "</simpleType>" + "</element>" + "</sequence>"
                        + "</complexType>", struct, true);
    }

    /**
     * A COBOL RENAMES clause. If should not emit anything. A warning is logged.
     */
    @Test
    public void testRename() {
        CobolDataItem struct = new CobolDataItem();
        CobolDataItem cobolItem = new CobolDataItem();
        cobolItem.setLevelNumber(66);
        struct.getChildren().add(cobolItem);
        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence/>"
                + "</complexType>", struct);
    }

    /**
     * A COBOL CONDITION clause with a single value, then 2. Check what happens
     * if a range is passed. Check that enumeration emission is controlled by
     * context option.
     */
    @Test
    public void testConditionSingleValue() {

        CobolDataItem struct = new CobolDataItem();

        CobolDataItem cobolItem = new CobolDataItem();
        cobolItem.setCobolName("COBOL-NAME");
        cobolItem.setPicture("X(6)");

        CobolDataItem cobolCondition = new CobolDataItem();
        cobolCondition.setLevelNumber(88);
        cobolCondition.setCobolName("A-CONDITION");
        List < String > literals = new ArrayList < String >();
        literals.add("avalue");
        cobolCondition.setConditionLiterals(literals);
        cobolItem.getChildren().add(cobolCondition);

        struct.getChildren().add(cobolItem);

        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"cobolName\">" + "<simpleType>"
                + "<restriction base=\"string\">" + "<maxLength value=\"6\"/>"
                + "<enumeration value=\"avalue\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct, false, true);

        literals.add("anotherValue");

        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"cobolName\">" + "<simpleType>"
                + "<restriction base=\"string\">" + "<maxLength value=\"6\"/>"
                + "<enumeration value=\"avalue\"/>"
                + "<enumeration value=\"anotherValue\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct, false, true);

        List < Range > conditionRanges = new ArrayList < Range >();
        conditionRanges.add(new Range("18", "56"));
        cobolCondition.setConditionRanges(conditionRanges);
        cobolCondition.getConditionLiterals().clear();

        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"cobolName\">" + "<simpleType>"
                + "<restriction base=\"string\">" + "<maxLength value=\"6\"/>"
                + "<minInclusive value=\"18\"/>"
                + "<maxInclusive value=\"56\"/>" + "</restriction>"
                + "</simpleType>" + "</element>" + "</sequence>"
                + "</complexType>", struct, false, true);

        emitAndCheck("<complexType name=\"Filler0\">" + "<sequence>"
                + "<element name=\"cobolName\">" + "<simpleType>"
                + "<restriction base=\"string\">" + "<maxLength value=\"6\"/>"
                + "</restriction>" + "</simpleType>" + "</element>"
                + "</sequence>" + "</complexType>", struct, false, false);

    }

    /**
     * Test an xsd:choice case.
     */
    @Test
    public void testChoice() {
        CobolDataItem struct = new CobolDataItem();

        CobolDataItem redefined = new CobolDataItem();
        redefined.setCobolName("COBOL-REDEFINED");
        struct.getChildren().add(redefined);

        CobolDataItem redefining = new CobolDataItem();
        redefining.setCobolName("COBOL-REDEFINING");
        redefining.setRedefines("COBOL-REDEFINED");
        struct.getChildren().add(redefining);

        emitAndCheck(
                "<complexType name=\"Filler0\">"
                        + "<sequence>"
                        + "<choice>"
                        + "<element name=\"cobolRedefined\" type=\"tns:CobolRedefined\"/>"
                        + "<element name=\"cobolRedefining\" type=\"tns:CobolRedefining\"/>"
                        + "</choice>" + "</sequence>" + "</complexType>"
                        + "<complexType name=\"CobolRedefined\">"
                        + "<sequence/>" + "</complexType>"
                        + "<complexType name=\"CobolRedefining\">"
                        + "<sequence/>" + "</complexType>", struct);

    }

    /**
     * Test an xsd:choice complex case.
     */
    @Test
    public void testChoiceComplex() {
        CobolDataItem a = new CobolDataItem("A");
        CobolDataItem b = new CobolDataItem("B");
        CobolDataItem c = new CobolDataItem("C");
        CobolDataItem d = new CobolDataItem("D");
        CobolDataItem e = new CobolDataItem("E");
        CobolDataItem f = new CobolDataItem("F");
        CobolDataItem g = new CobolDataItem("G");

        d.setRedefines("C");
        f.setRedefines("E");

        d.getChildren().add(e);
        d.getChildren().add(f);

        a.getChildren().add(b);
        a.getChildren().add(c);
        a.getChildren().add(d);
        a.getChildren().add(g);

        emitAndCheck("<complexType name=\"A\">" + "<sequence>"
                + "<element name=\"b\" type=\"tns:B\"/>" + "<choice>"
                + "<element name=\"c\" type=\"tns:C\"/>"
                + "<element name=\"d\" type=\"tns:D\"/>" + "</choice>"
                + "<element name=\"g\" type=\"tns:G\"/>" + "</sequence>"
                + "</complexType>" + "<complexType name=\"B\">" + "<sequence/>"
                + "</complexType>" + "<complexType name=\"C\">" + "<sequence/>"
                + "</complexType>" + "<complexType name=\"D\">" + "<sequence>"
                + "<choice>" + "<element name=\"e\" type=\"tns:E\"/>"
                + "<element name=\"f\" type=\"tns:F\"/>" + "</choice>"
                + "</sequence>" + "</complexType>" + "<complexType name=\"E\">"
                + "<sequence/>" + "</complexType>" + "<complexType name=\"F\">"
                + "<sequence/>" + "</complexType>" + "<complexType name=\"G\">"
                + "<sequence/>" + "</complexType>", a);

    }

    /**
     * @return a COBOL data item.
     * @param cobolName the desired COBOL name
     * @param picture the picture clause
     * @param usage the usage
     */
    private CobolDataItem getACobolElementaryItem(final String cobolName,
            final String picture, final Usage usage) {
        CobolDataItem cobolItem = new CobolDataItem();
        cobolItem.setCobolName(cobolName);
        cobolItem.setPicture(picture);
        cobolItem.setUsage(usage);
        return cobolItem;
    }

}
