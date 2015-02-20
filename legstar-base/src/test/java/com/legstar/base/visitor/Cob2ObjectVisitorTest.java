package com.legstar.base.visitor;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.gen.*;
import com.legstar.base.utils.HexUtils;
import com.legstar.base.visitor.Cob2ObjectVisitor;
import com.legstar.base.visitor.FromCobolChoiceStrategy;

public class Cob2ObjectVisitorTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testConvertFlat01() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                0);
        visitor.visit(new CobolFlat01Record());
        assertEquals("{comNumber=1043, comName=NAME000043, comAmount=2150.00}",
                visitor.getResultObject().toString());
        assertEquals(30, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat01Invalid() {
        try {
            Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                    cobolContext,
                    HexUtils.decodeHex("ABF0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                    0);
            visitor.visit(new CobolFlat01Record());
            fail();
        } catch (FromCobolException e) {
            assertEquals(
                    "Second nibble is not a digit."
                            + " Error at offset 0 : [0xABF0F1F0F4F3<-D5C1D4C5F0F0F0F0F4F3404040404040]."
                            + " COBOL variable path: FLAT01-RECORD/COM-NUMBER."
                            + " COBOL type: CobolZonedDecimalType",
                    e.getMessage());
        }

    }

    @Test
    public void testConvertFlat02() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E001F0014000F000C"),
                0);
        visitor.visit(new CobolFlat02Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[62, 31, 20, 15, 12]}",
                visitor.getResultObject().toString());
        assertEquals(40, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat02Invalid() {
        Cob2ObjectVisitor visitor;
        try {
            visitor = new Cob2ObjectVisitor(
                    cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E0F1F0014000F000C"),
                    0);
            visitor.visit(new CobolFlat02Record());
            fail();
        } catch (FromCobolException e) {
            assertEquals(
                    "Value 3871 is outside the required range [0, 99]."
                            + " Error at offset 32 : [0x404040404040404040400310000F003E->0F1F<-0014000F000C]."
                            + " COBOL variable path: FLAT02-RECORD[1]/COM-ARRAY."
                            + " COBOL type: CobolBinaryType", e.getMessage());
        }

    }

    @Test
    public void testConvertStru01() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2"),
                0);
        visitor.visit(new CobolStru01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comSubRecord={comItem1=62, comItem2=AB}}",
                visitor.getResultObject().toString());
        assertEquals(34, visitor.getLastPos());

    }

    @Test
    public void testConvertStru01Invalid() {
        try {
            Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                    cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F023EC1C2"),
                    0);
            visitor.visit(new CobolStru01Record());
            fail();
        } catch (FromCobolException e) {
            assertEquals(
                    "Value 574 is outside the required range [0, 99]."
                            + " Error at offset 30 : [0xF6F2404040404040404040400310000F->023E<-C1C2]."
                            + " COBOL variable path: STRU01-RECORD/COM-SUB-RECORD/COM-ITEM1."
                            + " COBOL type: CobolBinaryType", e.getMessage());
        }

    }

    @Test
    public void testConvertStru03() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C2000FC1C2000CC1C2"),
                0);
        visitor.visit(new CobolStru03Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[{comItem1=62, comItem2=AB}, {comItem1=31, comItem2=AB}, {comItem1=20, comItem2=AB}, {comItem1=15, comItem2=AB}, {comItem1=12, comItem2=AB}]}",
                visitor.getResultObject().toString());
        assertEquals(50, visitor.getLastPos());

    }

    @Test
    public void testConvertStru03Invalid() {
        try {
            Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                    cobolContext,
                    HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C20F0FC1C2000CC1C2"),
                    0);
            visitor.visit(new CobolStru03Record());
            fail();
        } catch (FromCobolException e) {
            assertEquals(
                    "Value 3855 is outside the required range [0, 99]."
                            + " Error at offset 42 : [0x0310000F003EC1C2001FC1C20014C1C2->0F0F<-C1C2000CC1C2]."
                            + " COBOL variable path: STRU03-RECORD[3]/COM-ARRAY/COM-ITEM1."
                            + " COBOL type: CobolBinaryType", e.getMessage());
        }

    }

    @Test
    public void testConvertArdo01EmptyVariableArray() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400000"),
                0);
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=0, comArray=[]}",
                visitor.getResultObject().toString());
        assertEquals(28, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01OneItemVariableArray() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400001000000000023556C"),
                0);
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=1, comArray=[235.56]}",
                visitor.getResultObject().toString());
        assertEquals(36, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01FullVariableArray() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400005000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0);
        visitor.visit(new CobolArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=5, comArray=[235.56, 236.56, 237.56, 238.56, 239.56]}",
                visitor.getResultObject().toString());
        assertEquals(68, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo02OdoObjectInRedefineFirstAlternative() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("0003C1C2C3"), 0);
        visitor.visit(new CobolArdo02Record());
        assertEquals(
                "{alternativeAChoice={alternativeA={odoCounter=3}}, odoArray=[{filler10=A}, {filler10=B}, {filler10=C}]}",
                visitor.getResultObject().toString());
        assertEquals(5, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo02OdoObjectInRedefineSecondAlternative() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("C1C2"), 0, new FromCobolChoiceStrategy() {

                    public CobolType choose(String choiceFieldName,
                            CobolChoiceType choiceType,
                            Map < String, Object > variables, byte[] hostData,
                            int start) {
                        return choiceType.getAlternatives().get("alternativeB");
                    }

                    public Set < String > getVariableNames() {
                        return null;
                    }

                });
        visitor.visit(new CobolArdo02Record());
        assertEquals(
                "{alternativeAChoice={alternativeB={filler8=AB}}, odoArray=[]}",
                visitor.getResultObject().toString());
        assertEquals(2, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03NoItemsAtAll() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0"), 0);
        visitor.visit(new CobolArdo03Record());
        assertEquals("{odoCounter=0, odoArray=[]}", visitor.getResultObject()
                .toString());
        assertEquals(5, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03OneOuterNoInners() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F1F0F0F0"), 0);
        visitor.visit(new CobolArdo03Record());
        assertEquals(
                "{odoCounter=1, odoArray=[{odoSubCounter=0, odoSubArray=[]}]}",
                visitor.getResultObject().toString());
        assertEquals(8, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03OneOuterOneInner() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F1F0F0F1C1C2C3C4"), 0);
        visitor.visit(new CobolArdo03Record());
        assertEquals(
                "{odoCounter=1, odoArray=[{odoSubCounter=1, odoSubArray=[{filler8=ABCD}]}]}",
                visitor.getResultObject().toString());
        assertEquals(12, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03OneOuterTwoInners() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F1F0F0F2C1C2C3C4C5C6C7C8"), 0);
        visitor.visit(new CobolArdo03Record());
        assertEquals(
                "{odoCounter=1, odoArray=[{odoSubCounter=2, odoSubArray=[{filler8=ABCD}, {filler8=EFGH}]}]}",
                visitor.getResultObject().toString());
        assertEquals(16, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03TwoOutersOneInnerEach() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F2F0F0F1C1C2C3C4F0F0F1C5C6C7C8"), 0);
        visitor.visit(new CobolArdo03Record());
        assertEquals(
                "{odoCounter=2, odoArray=[{odoSubCounter=1, odoSubArray=[{filler8=ABCD}]}, {odoSubCounter=1, odoSubArray=[{filler8=EFGH}]}]}",
                visitor.getResultObject().toString());
        assertEquals(19, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo03TwoOutersTwoInnersOneInner() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F2F0F0F2C1C2C3C4C5C6C7C8F0F0F1C9D1D2D3D4"),
                0);
        visitor.visit(new CobolArdo03Record());
        assertEquals(
                "{odoCounter=2, odoArray=[{odoSubCounter=2, odoSubArray=[{filler8=ABCD}, {filler8=EFGH}]}, {odoSubCounter=1, odoSubArray=[{filler8=IJKL}]}]}",
                visitor.getResultObject().toString());
        assertEquals(23, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategyFirstAlternative() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"), 0);
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail1={comName=12345}}}",
                visitor.getResultObject().toString());
        assertEquals(12, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategySecondAlternative() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("00010250000F"), 0);
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=1, comDetail1Choice={comDetail2={comAmount=2500.00}}}",
                visitor.getResultObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategy() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"), 0,
                new Rdef03ObjectFromHostChoiceStrategy());
        visitor.visit(new CobolRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice={comDetail3={comNumber=12345}}}",
                visitor.getResultObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef04DefaultStrategyFirstAlternative() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("c1c2c340404040404040e9"), 0);
        visitor.visit(new CobolRdef04Record());
        assertEquals(
                "{outerRedefinesLongChoice={outerRedefinesLong=ABC}, footer=Z}",
                visitor.getResultObject().toString());
        assertEquals(11, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef04CustomStrategyWithVariables() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("c1c2c300000000000000e9"), 0,
                new FromCobolChoiceStrategy() {

                    public CobolType choose(String choiceFieldName,
                            CobolChoiceType choiceType,
                            Map < String, Object > variables, byte[] hostData,
                            int start) {
                        if ("outerRedefinesLongChoice".equals(choiceFieldName)) {
                            return choiceType.getAlternatives().get(
                                    "outerRedefinesShort");
                        } else {
                            return choiceType.getAlternatives().get(
                                    "innerRedefinesShort");
                        }
                    }

                    public Set < String > getVariableNames() {
                        return null;
                    }

                });
        visitor.visit(new CobolRdef04Record());
        assertEquals(
                "{outerRedefinesLongChoice={outerRedefinesShort={innerRedefinesLongChoice={innerRedefinesShort=ABC}}}, footer=Z}",
                visitor.getResultObject().toString());
        assertEquals(11, visitor.getLastPos());

    }

    @Test
    public void testConvertCustdat() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5C"),
                0);
        visitor.visit(new CobolCustomerData());
        assertEquals(
                "{customerId=1, personalData={customerName=JOHN SMITH, customerAddress=CAMBRIDGE UNIVERSITY, customerPhone=44012565}, transactions={transactionNbr=2, transaction=[{transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}, {transactionDateChoice={transactionDate=10/04/11}, transactionAmount=235.56, transactionComment=*********}]}}",
                visitor.getResultObject().toString());
        assertEquals(108, visitor.getLastPos());

    }

    @Test
    public void testConvertStru04() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("0190000F00090006C2C5C5C2C4C40001900FC2C2C5C4C5C30000950F0003000000020013000CC2C4C2C1C5C40003800FC1C5C2C2C4C10001900F000600000005001C0013C1C5C2C5C1C30005700FC4C2C3C3C3C20002850F0009000000080023750F"),
                0);
        visitor.visit(new CobolStru04Record());
        assertEquals(
                "{comItem1=1900.00, comArray1=[{comItem2=9, comGroup1={comItem3=6, comArray2=[{comItem4=B, comArray3=[E, E, B, D, D], comItem5=19.00}, {comItem4=B, comArray3=[B, E, D, E, C], comItem5=9.50}], comItem6=3}, comItem7=2}, {comItem2=19, comGroup1={comItem3=12, comArray2=[{comItem4=B, comArray3=[D, B, A, E, D], comItem5=38.00}, {comItem4=A, comArray3=[E, B, B, D, A], comItem5=19.00}], comItem6=6}, comItem7=5}, {comItem2=28, comGroup1={comItem3=19, comArray2=[{comItem4=A, comArray3=[E, B, E, A, C], comItem5=57.00}, {comItem4=D, comArray3=[B, C, C, C, B], comItem5=28.50}], comItem6=9}, comItem7=8}], comItem8=237.50}",
                visitor.getResultObject().toString());
        assertEquals(98, visitor.getLastPos());

    }

    @Test
    public void testConvertAlltypes() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("c1c2c3c4" + "01020000" + "fc5c" + "000f"
                        + "0001343a" + "000001c4" + "0000000000004532456d"
                        + "0000000000007800056f"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000000564678008321f" + "000007545f"
                        + "45543ae9" + "361677a4590fab60" + "c1c2c3c4"
                        + "c1c2c3c4" + "40404040" + "40404040" + "fc5c"
                        + "fc5c" + "000f" + "000f" + "0001343a" + "0001343a"
                        + "000001c4" + "000001c4" + "0000000000004532456d"
                        + "0000000000004532456d" + "0000000000007800056f"
                        + "0000000000007800056f"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000087554907654321c"
                        + "0000000000000000000564678008321f"
                        + "0000000000000000000564678008321f" + "000007545f"
                        + "000007545f" + "45543ae9" + "45543ae9"
                        + "361677a4590fab60" + "361677a4590fab60"), 0);
        visitor.visit(new com.legstar.base.type.gen.alltypes.CobolDfhcommarea());
        assertEquals(
                "{sString=ABCD, sBinary=java.nio.HeapByteBuffer[pos=0 lim=4 cap=4], sShort=-932, sUshort=15, sInt=78906, sUint=452, sLong=-4532456, sUlong=7800056, sXlong=87554907654321, sUxlong=564678008321, sDec=75.45, sFloat=345006.56, sDouble=7.982006699999985E-14, aString=[ABCD, ABCD], aBinary=[, ], aShort=[-932, -932], aUshort=[15, 15], aInt=[78906, 78906], aUint=[452, 452], aLong=[-4532456, -4532456], aUlong=[7800056, 7800056], aXlong=[87554907654321, 87554907654321], aUxlong=[564678008321, 564678008321], aDec=[75.45, 75.45], aFloat=[345006.56, 345006.56], aDouble=[7.982006699999985E-14, 7.982006699999985E-14]}",
                visitor.getResultObject().toString());
        assertEquals(267, visitor.getLastPos());

    }

    @Test
    public void testConvertDplarcht() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("0000" + "5c404040" + "4040404040404040"
                        + "000000000f" + "0000" + "00000001"
                        + "c1c2c3c4c1c2c3c4" + "c4404040404040404040"
                        + "c4404040404040404040" + "c4404040404040404040"
                        + "c4404040404040404040" + "40404040"
                        + "c1c1c1c1c2c2c2c2c3c3c3c3"), 0);
        visitor.visit(new com.legstar.base.type.gen.dplarcht.CobolDfhcommarea());
        assertEquals(
                "{lsRequest={lsRequestType=0, lsAllItemsChoice={lsAllItems=*}, lsSearchCriteria={lsStartwith=, lsStartwithLen=0}}, lsReply={lsReplyType=0, lsReplyData={lsItemsCount=1, lsItemsArray=[{lsFilesDataChoice={lsFilesData={lsFileName=ABCDABCD, lsFileDsname=D         D         D         D, lsFileEnablestatus=AAAABBBBCCCC}}}]}}}",
                visitor.getResultObject().toString());
        assertEquals(89, visitor.getLastPos());

    }

    @Test
    public void testConvertDplarchtTransactionChoice() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("0002"
                        + "5c404040"
                        + "4040404040404040"
                        + "000000000f"
                        + "0001"
                        + "00000001"
                        + "c1c1c1c1c1c1c1c1"
                        + "c2c2c2c2c2c2c2c2"
                        + "c3c3c3c3c3c3c3c3c3c3c3c3"
                        + "404040404040404040404040404040404040404040404040404040404040404040404040"),
                0, new DplarchtChoiceStrategy());
        visitor.visit(new com.legstar.base.type.gen.dplarcht.CobolDfhcommarea());
        assertEquals(
                "{lsRequest={lsRequestType=2, lsAllItemsChoice={lsAllItems=*}, lsSearchCriteria={lsStartwith=, lsStartwithLen=0}}, lsReply={lsReplyType=1, lsReplyData={lsItemsCount=1, lsItemsArray=[{lsFilesDataChoice={lsTransactionsData={lsTransactionName=AAAAAAAA, lsTransactionProgram=BBBBBBBB, lsTransactionStatus=CCCCCCCCCCCC, filler119=}}}]}}}",
                visitor.getResultObject().toString());
        assertEquals(89, visitor.getLastPos());

    }

    @Test
    public void testConvertDplarchtProgramChoice() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("0001" + "5c404040" + "4040404040404040"
                        + "000000000f" + "0000" + "00000001"
                        + "c2c9d5c1d9c3c8e3" + "d7d9d6c7d9c1d44040404040"
                        + "d5d6e3c4c5c6c9d5c5c44040" + "000016a0" + "00000002"
                        + "404040404040404040404040404040404040404040404040"),
                0, new DplarchtChoiceStrategy());
        visitor.visit(new com.legstar.base.type.gen.dplarcht.CobolDfhcommarea());
        assertEquals(
                "{lsRequest={lsRequestType=1, lsAllItemsChoice={lsAllItems=*}, lsSearchCriteria={lsStartwith=, lsStartwithLen=0}}, lsReply={lsReplyType=0, lsReplyData={lsItemsCount=1, lsItemsArray=[{lsFilesDataChoice={lsProgramsData={lsProgramName=BINARCHT, lsProgramType=PROGRAM, lsProgramLanguage=NOTDEFINED, lsProgramLength=5792, lsProgramUsecount=2, filler113=}}}]}}}",
                visitor.getResultObject().toString());
        assertEquals(89, visitor.getLastPos());

    }

    private class DplarchtChoiceStrategy implements FromCobolChoiceStrategy {
        public CobolType choose(String choiceFieldName,
                CobolChoiceType choiceType, Map < String, Object > variables,
                byte[] hostData, int start) {
            int select = ((Number) variables.get("lsRequestType")).intValue();

            switch (select) {
            case 0:
                return choiceType.getAlternatives().get("lsFilesData");
            case 1:
                return choiceType.getAlternatives().get("lsProgramsData");
            case 2:
                return choiceType.getAlternatives().get("lsTransactionsData");
            default:
                return null;

            }
        }

        public Set < String > getVariableNames() {
            Set < String > varNames = new HashSet < String >();
            varNames.add("lsRequestType");
            return varNames;
        }
    }

    @Test
    public void testConvertOpt01AllAbsent() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F0"), 0);
        visitor.visit(new CobolOptl01Record());
        assertEquals("{optlStructInd=0, optlItemInd=0}", visitor
                .getResultObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertOpt01StructPresentStringAbsent() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F1F0F0F0F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8C1C2C3C4C5"),
                0);
        visitor.visit(new CobolOptl01Record());
        assertEquals(
                "{optlStructInd=1, optlItemInd=0, optlStruct={optlStructField1=123456789012345678, optlStructField2=ABCDE}}",
                visitor.getResultObject().toString());
        assertEquals(29, visitor.getLastPos());

    }

    @Test
    public void testConvertOpt01StructAbsentStringPresent() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D2D3"),
                0);
        visitor.visit(new CobolOptl01Record());
        assertEquals(
                "{optlStructInd=0, optlItemInd=1, optlItem=JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJKL}",
                visitor.getResultObject().toString());
        assertEquals(38, visitor.getLastPos());

    }

    @Test
    public void testConvertOpt01StructPresentStringPresent() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F0F0F1F0F0F1F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8C1C2C3C4C5D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D2D3"),
                0);
        visitor.visit(new CobolOptl01Record());
        assertEquals(
                "{optlStructInd=1, optlItemInd=1, optlStruct={optlStructField1=123456789012345678, optlStructField2=ABCDE}, optlItem=JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJKL}",
                visitor.getResultObject().toString());
        assertEquals(61, visitor.getLastPos());

    }

    @Test
    public void testConvertCflt01() {
        Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(
                cobolContext,
                HexUtils.decodeHex("F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8C1C2C3C4C5C1C2C3C4C5C6C7C8C9C0C1C2C3C4C5C6C7C8D1D2D3D4D5"),
                0);
        visitor.visit(new CobolCflt01Record());
        assertEquals(
                "{cfltParent1={cfltInfo={cfltId=123456789012345678, cfltTypCd=ABCDE}}, cfltParent2={cfltInfo={cfltIdCt=ABCDEFGHI{ABCDEFGH, cfltTypCdCt=JKLMN}}}",
                visitor.getResultObject().toString());
        assertEquals(46, visitor.getLastPos());

    }

}
