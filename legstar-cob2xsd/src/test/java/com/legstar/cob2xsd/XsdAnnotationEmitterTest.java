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

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.junit.Test;

import com.legstar.cob2xsd.antlr.RecognizerErrorHandler;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolUsage.Usage;

/**
 * Test the XSD annotations produced.
 * 
 */
public class XsdAnnotationEmitterTest extends AbstractXsdEmitterTester {

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * Test a group item.
     */
    @Test
    public void testGroupItem() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.getChildren().add(new CobolDataItem("CHILD-NAME"));
        emitAnnotationAndCheck(
                dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\" levelNumber=\"1\" type=\"GROUP_ITEM\"/>");
    }

    /**
     * Test an elementary item with usage.
     */
    @Test
    public void testUsage() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setUsage(Usage.DOUBLEFLOAT);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " type=\"DOUBLE_FLOAT_ITEM\""
                        + " usage=\"COMP-2\"/>");
    }

    /**
     * Test an elementary item with picture.
     */
    @Test
    public void testPicture() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("NNNN/NN");
        dataItem.setUsage(Usage.NATIONAL);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"NNNN/NN\""
                        + " type=\"NATIONAL_ITEM\"" + " usage=\"NATIONAL\"/>");
    }

    /**
     * Test an elementary item with justified right.
     */
    @Test
    public void testJustifiedRight() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X");
        dataItem.setJustifiedRight(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X\""
                        + " type=\"ALPHANUMERIC_ITEM\""
                        + " justifiedRight=\"true\"/>");
    }

    /**
     * Test an elementary item with total digits.
     */
    @Test
    public void testTotalDigits() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("99");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"99\""
                        + " type=\"PACKED_DECIMAL_ITEM\""
                        + " usage=\"PACKED-DECIMAL\"" + " totalDigits=\"2\""
                        + " signed=\"false\"" + "/>");
    }

    /**
     * Test an elementary item with fraction digits.
     */
    @Test
    public void testFractionDigits() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"S99V9\""
                        + " type=\"ZONED_DECIMAL_ITEM\"" + " totalDigits=\"3\""
                        + " fractionDigits=\"1\"" + " signed=\"true\"" + "/>");
    }

    /**
     * Test an elementary item with sign leading.
     */
    @Test
    public void testSignLeading() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        dataItem.setSignLeading(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"S99V9\""
                        + " type=\"ZONED_DECIMAL_ITEM\"" + " totalDigits=\"3\""
                        + " fractionDigits=\"1\"" + " signed=\"true\""
                        + " signLeading=\"true\"" + "/>");
    }

    /**
     * Test an elementary item with sign separate.
     */
    @Test
    public void testSignSeparate() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        dataItem.setSignLeading(true);
        dataItem.setSignSeparate(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"S99V9\""
                        + " type=\"ZONED_DECIMAL_ITEM\"" + " totalDigits=\"3\""
                        + " fractionDigits=\"1\"" + " signed=\"true\""
                        + " signLeading=\"true\"" + " signSeparate=\"true\""
                        + "/>");
    }

    /**
     * Test an array item.
     */
    @Test
    public void testArray() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setMaxOccurs(3);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " maxOccurs=\"3\""
                        + " minOccurs=\"3\"" + " type=\"GROUP_ITEM\"" + "/>");
    }

    /**
     * Test an array item with depending on.
     */
    @Test
    public void testArrayDependingOn() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setMinOccurs(0);
        dataItem.setMaxOccurs(1);
        dataItem.setDependingOn("COBOL-LEN");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " dependingOn=\"COBOL-LEN\""
                        + " maxOccurs=\"1\"" + " minOccurs=\"0\""
                        + " type=\"GROUP_ITEM\"" + "/>");
    }

    /**
     * Test an item redefining another.
     */
    @Test
    public void testRedefines() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-REDEFINING");
        dataItem.setRedefines("COBOL-REDEFINED");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-REDEFINING\""
                        + " levelNumber=\"1\""
                        + " redefines=\"COBOL-REDEFINED\""
                        + " type=\"GROUP_ITEM\"" + "/>");
    }

    /**
     * Test an item with value ZERO.
     */
    @Test
    public void testValueZERO() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setValue("ZEROS");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " value=\"0\""
                        + " type=\"GROUP_ITEM\"" + "/>");
    }

    /**
     * Test an item with value SPACE.
     */
    @Test
    public void testValueSPACE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setValue("SPACE");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " value=\" \""
                        + " type=\"GROUP_ITEM\"" + "/>");
    }

    /**
     * Test an item with value HIGH-VALUE.
     */
    @Test
    public void testValueHIGHVALUE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("HIGH-VALUES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"0xFFFFFF\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value LOW-VALUE.
     */
    @Test
    public void testValueLOWVALUE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("LOW-VALUES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"0x000000\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value LOW-VALUE.
     */
    @Test
    public void testValueNULLS() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("NULLS");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"0x000000\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value QUOTES.
     */
    @Test
    public void testValueQUOTES() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("QUOTES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"&quot;\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value APOST.
     */
    @Test
    public void testValueAPOST() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("apost");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"&apos;\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value ALL.
     */
    @Test
    public void testValueALL() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("ALL A");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"AAA\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Test an item with value ALL applied to a figurative constant.
     */
    @Test
    public void testValueALLFigurative() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("ALL QUOTE");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"&quot;&quot;&quot;\""
                        + " type=\"ALPHANUMERIC_ITEM\"" + "/>");
    }

    /**
     * Test an item with a numeric.
     */
    @Test
    public void testValueNumeric() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        dataItem.setValue("-125.63");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " usage=\"PACKED-DECIMAL\""
                        + " value=\"-125.63\""
                        + " type=\"PACKED_DECIMAL_ITEM\"" + "/>");
    }

    /**
     * Test an item with a alpha escaped.
     */
    @Test
    public void testValueEscaped() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("<A>");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"&lt;A&gt;\""
                        + " type=\"ALPHANUMERIC_ITEM\"" + "/>");
    }

    /**
     * Test an item with a value that is a delimited literal.
     */
    @Test
    public void testValueDelimitedLiteral() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.setValue("\"ABD\"");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                        + " levelNumber=\"1\"" + " picture=\"X(3)\""
                        + " value=\"ABD\"" + " type=\"ALPHANUMERIC_ITEM\""
                        + "/>");
    }

    /**
     * Helper to check item level annotation emission. Get item level
     * annotations. We hook them directly in the schema because that simplifies
     * testing.
     * 
     * @param dataItem a COBOL data item
     * @param expected expected annotations as a string
     */
    private void emitAnnotationAndCheck(final CobolDataItem dataItem,
            final String expected) {
        configProps.put(Cob2XsdConfig.ADD_LEGSTAR_ANNOTATIONS,
                Boolean.toString(true));
        Cob2XsdConfig config = new Cob2XsdConfig(configProps);
        XmlSchema xsd = getXmlSchema();
        XsdAnnotationEmitter emitter = new XsdAnnotationEmitter(xsd, config);
        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, config, null, 0,
                new ArrayList < String >(), _errorHandler);
        XmlSchemaAnnotation annotation = emitter
                .createLegStarAnnotation(xsdDataItem);
        xsd.setAnnotation(annotation);
        check("<annotation>" + "<appinfo>" + expected + "</appinfo>"
                + "</annotation>", xsd, true);
    }

}
