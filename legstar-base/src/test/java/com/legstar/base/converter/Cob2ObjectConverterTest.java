package com.legstar.base.converter;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.converter.Cob2ObjectConverter;
import com.legstar.base.type.ConversionException;
import com.legstar.base.type.gen.*;
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
        visitor.visit(new CobolFlat01Record());
        assertEquals("{comNumber=1043, comName=NAME000043, comAmount=2150.00}",
                visitor.getLastObject().toString());
        assertEquals(30, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat01Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("ABF0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                    0);
            visitor.visit(new CobolFlat01Record());
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
        visitor.visit(new CobolFlat02Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[62, 31, 20, 15, 12]}",
                visitor.getLastObject().toString());
        assertEquals(40, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat02Invalid() {
        Cob2ObjectConverter visitor;
        try {
            visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E0F1F0014000F000C"),
                    0);
            visitor.visit(new CobolFlat02Record());
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
        visitor.visit(new CobolStru01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comSubRecord={comItem1=62, comItem2=AB}}",
                visitor.getLastObject().toString());
        assertEquals(34, visitor.getLastPos());

    }

    @Test
    public void testConvertStru01Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F023EC1C2"),
                    0);
            visitor.visit(new CobolStru01Record());
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
        visitor.visit(new CobolStru03Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[{comItem1=62, comItem2=AB}, {comItem1=31, comItem2=AB}, {comItem1=20, comItem2=AB}, {comItem1=15, comItem2=AB}, {comItem1=12, comItem2=AB}]}",
                visitor.getLastObject().toString());
        assertEquals(50, visitor.getLastPos());

    }

    @Test
    public void testConvertStru03Invalid() {
        try {
            Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C20F0FC1C2000CC1C2"),
                    0);
            visitor.visit(new CobolStru03Record());
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
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=0, comArray=[]}",
                visitor.getLastObject().toString());
        assertEquals(28, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01OneItemVariableArray() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400001000000000023556C"),
                0);
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=1, comArray=[235.56]}",
                visitor.getLastObject().toString());
        assertEquals(36, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01FullVariableArray() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400005000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0);
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=5, comArray=[235.56, 236.56, 237.56, 238.56, 239.56]}",
                visitor.getLastObject().toString());
        assertEquals(68, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategyFirstAlternative() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0);
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail1={comName=12345}}}",
                visitor.getLastObject().toString());
        assertEquals(12, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategySecondAlternative() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("00010250000F"),
                0);
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=1, comDetail1Choice={comDetail2={comAmount=2500.00}}}",
                visitor.getLastObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategy() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0, new Rdef03ObjectFromHostChoiceStrategy());
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail3={comNumber=12345}}}",
                visitor.getLastObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategyWithVariables() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0, new Rdef03ObjectFromHostChoiceStrategy(), Arrays.asList(new String[] {"comSelect"}));
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail3={comNumber=12345}}}",
                visitor.getLastObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertCustdat() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5C"),
                0);
        visitor.visit(new CobolCustomerData());
        assertEquals(
                "{customerId=1, personalData={customerName=JOHN SMITH, customerAddress=CAMBRIDGE UNIVERSITY, customerPhone=44012565}, transactions={transactionNbr=2, transaction=[{transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}, {transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}]}}",
                visitor.getLastObject().toString());
        assertEquals(108, visitor.getLastPos());

    }

    @Test
    public void testConvertStru04() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex("0190000F00090006C2C5C5C2C4C40001900FC2C2C5C4C5C30000950F0003000000020013000CC2C4C2C1C5C40003800FC1C5C2C2C4C10001900F000600000005001C0013C1C5C2C5C1C30005700FC4C2C3C3C3C20002850F0009000000080023750F"),
                0);
        visitor.visit(new CobolStru04Record());
        assertEquals(
                "{ComItem1=1900.00, ComArray1=[{ComItem2=9, ComGroup1={ComItem3=6, ComArray2=[{ComItem4=B, ComArray3=[E, E, B, D, D], ComItem5=19.00}, {ComItem4=B, ComArray3=[B, E, D, E, C], ComItem5=9.50}], ComItem6=3}, ComItem7=2}, {ComItem2=19, ComGroup1={ComItem3=12, ComArray2=[{ComItem4=B, ComArray3=[D, B, A, E, D], ComItem5=38.00}, {ComItem4=A, ComArray3=[E, B, B, D, A], ComItem5=19.00}], ComItem6=6}, ComItem7=5}, {ComItem2=28, ComGroup1={ComItem3=19, ComArray2=[{ComItem4=A, ComArray3=[E, B, E, A, C], ComItem5=57.00}, {ComItem4=D, ComArray3=[B, C, C, C, B], ComItem5=28.50}], ComItem6=9}, ComItem7=8}], ComItem8=237.50}",
                visitor.getLastObject().toString());
        assertEquals(98, visitor.getLastPos());

    }

    @Test
    public void testConvertAlltypes() {
        Cob2ObjectConverter visitor = new Cob2ObjectConverter(cobolContext,
                HexUtils.decodeHex(                "c1c2c3c4"
                        + "01020000"
                        + "fc5c"
                        + "000f"
                        + "0001343a"
                        + "000001c4"
                        + "0000000000004532456d"
                        + "0000000000007800056f"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000000564678008321f"
                        + "000007545f"
                        + "45543ae9"
                        + "361677a4590fab60"
                        + "c1c2c3c4"
                        + "c1c2c3c4"
                        + "40404040"
                        + "40404040"
                        + "fc5c"
                        + "fc5c"
                        + "000f"
                        + "000f"
                        + "0001343a"
                        + "0001343a"
                        + "000001c4"
                        + "000001c4"
                        + "0000000000004532456d"
                        + "0000000000004532456d"
                        + "0000000000007800056f"
                        + "0000000000007800056f"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000000564678008321f"
                        + "0000000000000000000564678008321f"
                        + "000007545f"
                        + "000007545f"
                        + "45543ae9"
                        + "45543ae9"
                        + "361677a4590fab60"
                        + "361677a4590fab60"),
                0);
        visitor.visit(new CobolDfhcommarea());
        assertEquals(
                "{SString=ABCD, SBinary=java.nio.HeapByteBuffer[pos=0 lim=4 cap=4], SShort=-932, SUshort=15, SInt=78906, SUint=452, SLong=-4532456, SUlong=7800056, SXlong=87554907654321, SUxlong=564678008321, SDec=75.45, SFloat=345006.56, SDouble=7.982006699999985E-14, AString=[ABCD, ABCD], ABinary=[java.nio.HeapByteBuffer[pos=0 lim=4 cap=4], java.nio.HeapByteBuffer[pos=0 lim=4 cap=4]], AShort=[-932, -932], AUshort=[15, 15], AInt=[78906, 78906], AUint=[452, 452], ALong=[-4532456, -4532456], AUlong=[7800056, 7800056], AXlong=[87554907654321, 87554907654321], AUxlong=[564678008321, 564678008321], ADec=[75.45, 75.45], AFloat=[345006.56, 345006.56], ADouble=[7.982006699999985E-14, 7.982006699999985E-14]}",
                visitor.getLastObject().toString());
        assertEquals(267, visitor.getLastPos());

    }

}
