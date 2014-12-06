package com.legstar.jaxb.converter;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Map;

import legstar.test.converter.Ardo01RecordFactory;
import legstar.test.converter.CustomerDataFactory;
import legstar.test.converter.Flat01RecordFactory;
import legstar.test.converter.Flat02RecordFactory;
import legstar.test.converter.Rdef01RecordFactory;
import legstar.test.converter.Rdef02RecordFactory;
import legstar.test.converter.Rdef03RecordFactory;
import legstar.test.converter.Stru01RecordFactory;
import legstar.test.converter.Stru03RecordFactory;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.utils.HexUtils;
import com.legstar.base.visitor.FromCobolChoiceStrategy;
import com.legstar.jaxb.converter.Cob2JaxbConverter;
import com.legstar.jaxb.converter.gen.Ardo01RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.CustomerDataJaxbFactory;
import com.legstar.jaxb.converter.gen.Flat01RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Flat02RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Rdef01RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Rdef02RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Rdef03RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Stru01RecordJaxbFactory;
import com.legstar.jaxb.converter.gen.Stru03RecordJaxbFactory;

public class Cob2JaxbConverterTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testConvertFlat01() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                0, new Flat01RecordJaxbFactory());
        visitor.visit(Flat01RecordFactory.create());
        assertEquals("{comNumber=1043, comName=NAME000043, comAmount=2150.00}",
                visitor.getLastObject().toString());
        assertEquals(30, visitor.getLastPos());

    }

    @Test
    public void testConvertFlat02() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E001F0014000F000C"),
                0, new Flat02RecordJaxbFactory());
        visitor.visit(Flat02RecordFactory.create());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[62, 31, 20, 15, 12]}",
                visitor.getLastObject().toString());
        assertEquals(40, visitor.getLastPos());

    }

    @Test
    public void testConvertStru01() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2"),
                0, new Stru01RecordJaxbFactory());
        visitor.visit(Stru01RecordFactory.create());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comSubRecord={comItem1=62, comItem2=AB}}",
                visitor.getLastObject().toString());
        assertEquals(34, visitor.getLastPos());

    }

    @Test
    public void testConvertStru03() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C2000FC1C2000CC1C2"),
                0, new Stru03RecordJaxbFactory());
        visitor.visit(Stru03RecordFactory.create());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comAmount=3100.00, comArray=[{comItem1=62, comItem2=AB}, {comItem1=31, comItem2=AB}, {comItem1=20, comItem2=AB}, {comItem1=15, comItem2=AB}, {comItem1=12, comItem2=AB}]}",
                visitor.getLastObject().toString());
        assertEquals(50, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef01DefaultStrategy() {
        CobolComplexType type = Rdef01RecordFactory.create();
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("0000D5C1D4C5F0F0F0F0F0F500010250000F"), 0,
                new Rdef01RecordJaxbFactory());
        visitor.visit(type);
        assertEquals(
                "{comSelect=0, comDetail1Choice=comDetail1={comName=NAME000005}}",
                visitor.getLastObject().toString());
        assertEquals(12, visitor.getLastPos());

        visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("0000D5C1D4C5F0F0F0F0F0F500010250000F"), 12,
                new Rdef01RecordJaxbFactory());
        visitor.visit(type);
        assertEquals(
                "{comSelect=1, comDetail1Choice=comDetail2={comAmount=2500.00}}",
                visitor.getLastObject().toString());
        assertEquals(18, visitor.getLastPos());
    }

    @Test
    public void testConvertRdef01CustomStrategy() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("00010250000F40404040404000010260000F404040404040"),
                0, new Rdef01RecordJaxbFactory(),
                new FromCobolChoiceStrategy() {

                    public CobolType choose(String choiceFieldName,
                            CobolChoiceType choiceType,
                            Map < String, Object > variables, byte[] hostData,
                            int start) {

                        int select = ((Number) variables.get("comSelect"))
                                .intValue();

                        switch (select) {
                        case 0:
                            return choiceType.getAlternatives().get(
                                    "comDetail1");
                        case 1:
                            return choiceType.getAlternatives().get(
                                    "comDetail2");
                        default:
                            return null;

                        }

                    }
                }, Arrays.asList(new String[] { "comSelect" }));
        visitor.visit(Rdef01RecordFactory.create());
        assertEquals(
                "{comSelect=1, comDetail1Choice=comDetail2={comAmount=2500.00}}",
                visitor.getLastObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef02DefaultStrategy() {
        CobolComplexType type = Rdef02RecordFactory.create();
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("C1C2C3D1D2D30000D5C1D4C5F0F0F0F0F0F50260000F"),
                0, new Rdef02RecordJaxbFactory());
        visitor.visit(type);
        assertEquals(
                "{rdef02Key={rdef02Item1Choice=rdef02Item2=ABCJKL, comSelect=0}, comDetail1Choice=comDetail1={comName=NAME000005}, comItem3=2600.00}",
                visitor.getLastObject().toString());
        assertEquals(22, visitor.getLastPos());

        visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("00001361588C0001D5C1D4C5F0F0F0F0F0F50261588F"),
                0, new Rdef02RecordJaxbFactory());
        visitor.visit(type);
        assertEquals(
                "{rdef02Key={rdef02Item1Choice=rdef02Item1=1361588, comSelect=1}, comDetail1Choice=comDetail1={comName=NAME000005}, comItem3=2615.88}",
                visitor.getLastObject().toString());
        assertEquals(22, visitor.getLastPos());
    }

    @Test
    public void testConvertRdef03DefaultStrategyFirstAlternative() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"), 0,
                new Rdef03RecordJaxbFactory());
        visitor.visit(Rdef03RecordFactory.createRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice=comDetail1={comName=12345}}",
                visitor.getLastObject().toString());
        assertEquals(12, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03DefaultStrategySecondAlternative() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("00010250000F"), 0,
                new Rdef03RecordJaxbFactory());
        visitor.visit(Rdef03RecordFactory.createRdef03Record());
        assertEquals(
                "{comSelect=1, comDetail1Choice=comDetail2={comAmount=2500.00}}",
                visitor.getLastObject().toString());
        assertEquals(6, visitor.getLastPos());

    }

    @Test
    public void testConvertRdef03CustomStrategyWithVariables() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"), 0,
                new Rdef03RecordJaxbFactory(),
                new Rdef03ObjectFromHostChoiceStrategy(),
                Arrays.asList(new String[] { "comSelect" }));
        visitor.visit(Rdef03RecordFactory.createRdef03Record());
        assertEquals(
                "{comSelect=2, comDetail1Choice=comDetail3={comNumber=12345}}",
                visitor.getLastObject().toString());
        assertEquals(7, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01EmptyVariableArray() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400000"),
                0, new Ardo01RecordJaxbFactory());
        visitor.visit(Ardo01RecordFactory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=0, comArray=[]}",
                visitor.getLastObject().toString());
        assertEquals(28, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01OneItemVariableArray() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400001000000000023556C"),
                0, new Ardo01RecordJaxbFactory());
        visitor.visit(Ardo01RecordFactory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=1, comArray=[235.56]}",
                visitor.getLastObject().toString());
        assertEquals(36, visitor.getLastPos());

    }

    @Test
    public void testConvertArdo01FullVariableArray() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(
                cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400005000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0, new Ardo01RecordJaxbFactory());
        visitor.visit(Ardo01RecordFactory.createArdo01Record());
        assertEquals(
                "{comNumber=62, comName=NAME000062, comNbr=5, comArray=[235.56, 236.56, 237.56, 238.56, 239.56]}",
                visitor.getLastObject().toString());
        assertEquals(68, visitor.getLastPos());

    }

    
    @Test
    public void testConvertCustdat() {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5C"),
                0, new CustomerDataJaxbFactory());
        visitor.visit(CustomerDataFactory.createCustomerData());
        assertEquals(
                "{customerId=1, personalData={customerName=JOHN SMITH, customerAddress=CAMBRIDGE UNIVERSITY, customerPhone=44012565}, transactions={transactionNbr=2, transaction=[{transactionDateChoice=transactionDate=10/04/11, transactionAmount=235.56, transactionComment=*********}, {transactionDateChoice=transactionDate=10/04/11, transactionAmount=235.56, transactionComment=*********}]}}",
                visitor.getLastObject().toString());
        assertEquals(108, visitor.getLastPos());

    }

    private class Rdef03ObjectFromHostChoiceStrategy implements
            FromCobolChoiceStrategy {

        public CobolType choose(String choiceFieldName,
                CobolChoiceType choiceType, Map < String, Object > variables,
                byte[] hostData, int start) {

            int select = ((Number) variables.get("comSelect")).intValue();

            switch (select) {
            case 0:
                return choiceType.getAlternatives().get("comDetail1");
            case 1:
                return choiceType.getAlternatives().get("comDetail2");
            case 2:
                return choiceType.getAlternatives().get("comDetail3");
            default:
                return null;

            }
        }

    }

}
