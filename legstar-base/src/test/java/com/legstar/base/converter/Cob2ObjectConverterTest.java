package com.legstar.base.converter;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.converter.Cob2ObjectConverter;
import com.legstar.base.type.ConversionException;
import com.legstar.base.type.gen.Ardo01Factory;
import com.legstar.base.type.gen.CustdatFactory;
import com.legstar.base.type.gen.Flat01Factory;
import com.legstar.base.type.gen.Flat02Factory;
import com.legstar.base.type.gen.Rdef03Factory;
import com.legstar.base.type.gen.Stru01Factory;
import com.legstar.base.type.gen.Stru03Factory;
import com.legstar.base.utils.HexUtils;
import com.legstar.base.visitor.Rdef03ObjectFromHostChoiceStrategy;

public class Cob2ObjectConverterTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testConvertFlat01() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                0);
        visitor.visit(Flat01Factory.create());
        assertEquals("{comNumber=1043, comName=NAME000043, comAmount=2150.00}",
                visitor.getObject().toString());
        assertEquals(30, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat01Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("ABF0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                    0);
            visitor.visit(Flat01Factory.create());
            fail();
        } catch (ConversionException e) {
            assertEquals(
                    "Nibble is not a digit. Position is 0. Data at position 0x->ABF0F1F0F4F3D5C1D4C5F0F0F0F0F4F3",
                    e.getMessage());
        }

    }

    @Test
    public void testConvertFlat02() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E001F0014000F000C"),
                0);
        visitor.visit(Flat02Factory.create());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[62, 31, 20, 15, 12]}",
                visitor.getObject().toString());
        assertEquals(40, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat02Invalid() {
        Cob2ObjectConverter visitor;
        try {
            visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E0F1F0014000F000C"),
                    0);
            visitor.visit(Flat02Factory.create());
            fail();
        } catch (ConversionException e) {
            assertEquals(
                    "Value 3871 is outside the required range [0, 99]."
                            + " Position is 32."
                            + " Data at position 0x404040404040404040400310000F003E->0F1F0014000F000C",
                    e.getMessage());
        }

    }

    @Test
    public void testConvertStru01() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2"),
                0);
        visitor.visit(Stru01Factory.createStru01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comSubRecord={comItem1=62, comItem2=AB}}",
                visitor.getObject().toString());
        assertEquals(34, visitor.getLastPos());

    }

    @Test
    public void testConvertStru01Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F023EC1C2"),
                    0);
            visitor.visit(Stru01Factory.createStru01Record());
            fail();
        } catch (ConversionException e) {
            assertEquals(
                    "Value 574 is outside the required range [0, 99]." +
                    " Position is 30." +
                    " Data at position 0xF6F2404040404040404040400310000F->023EC1C2",
                    e.getMessage());
        }

    }

    @Test
    public void testConvertStru03() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C2000FC1C2000CC1C2"),
                0);
        visitor.visit(Stru03Factory.createStru03Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[{comItem1=62, comItem2=AB}, {comItem1=31, comItem2=AB}, {comItem1=20, comItem2=AB}, {comItem1=15, comItem2=AB}, {comItem1=12, comItem2=AB}]}",
                visitor.getObject().toString());
        assertEquals(50, visitor.getLastPos());

    }

    @Test
    public void testConvertStru03Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C20F0FC1C2000CC1C2"),
                    0);
            visitor.visit(Stru03Factory.createStru03Record());
            fail();
        } catch (ConversionException e) {
            assertEquals(
                    "Value 3855 is outside the required range [0, 99]." +
                    " Position is 42." +
                    " Data at position 0x0310000F003EC1C2001FC1C20014C1C2->0F0FC1C2000CC1C2",
                    e.getMessage());
        }

    }


    @Test
    public void testConvertArdo01EmptyVariableArray() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400000"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=0, comArray=[]}",
                visitor.getObject().toString());
        assertEquals(28, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01OneItemVariableArray() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400001000000000023556C"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=1, comArray=[235.56]}",
                visitor.getObject().toString());
        assertEquals(36, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01FullVariableArray() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400005000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=5, comArray=[235.56, 236.56, 237.56, 238.56, 239.56]}",
                visitor.getObject().toString());
        assertEquals(68, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategyFirstAlternative() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0);
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail1={comName=12345}}}",
                visitor.getObject().toString());
        assertEquals(12, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategySecondAlternative() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("00010250000F"),
                0);
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertEquals(
                "{comSelect=1, comDetail1Choice={comDetail2={comAmount=2500.00}}}",
                visitor.getObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategy() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0, new Rdef03ObjectFromHostChoiceStrategy());
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail3={comNumber=12345}}}",
                visitor.getObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategyWithVariables() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0, new Rdef03ObjectFromHostChoiceStrategy(), Arrays.asList(new String[] {"comSelect"}));
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail3={comNumber=12345}}}",
                visitor.getObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertCustdat() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5C"),
                0);
        visitor.visit(CustdatFactory.createCustomerDataCobolType());
        assertEquals(
                "{customerId=1, personalData={customerName=JOHN SMITH, customerAddress=CAMBRIDGE UNIVERSITY, customerPhone=44012565}, transactions={transactionNbr=2, transaction=[{transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}, {transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}]}}",
                visitor.getObject().toString());
        assertEquals(108, visitor.getLastPos());

    }

}
