package com.legstar.converter.visitor;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.legstar.converter.context.CobolContext;
import com.legstar.converter.context.EbcdicCobolContext;
import com.legstar.converter.type.gen.Ardo01Factory;
import com.legstar.converter.type.gen.CustdatFactory;
import com.legstar.converter.type.gen.Flat01Factory;
import com.legstar.converter.type.gen.Flat02Factory;
import com.legstar.converter.type.gen.Rdef03Factory;
import com.legstar.converter.type.gen.Stru01Factory;
import com.legstar.converter.type.gen.Stru03Factory;
import com.legstar.converter.utils.HexUtils;
import com.legstar.converter.visitor.ValidateFromCobolVisitor;

public class ValidateFromCobolVisitorTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void tesFlat01Valid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                0);
        visitor.visit(Flat01Factory.create());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesFlat01Invalid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0FFF0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"),
                0);
        visitor.visit(Flat01Factory.create());
        assertFalse(visitor.isValid());

        visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F4F3D5C101C5F0F0F0F0F4F3404040404040404040400215000F"),
                0);
        visitor.visit(Flat01Factory.create());
        assertFalse(visitor.isValid());


        visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215A00F"),
                0);
        visitor.visit(Flat01Factory.create());
        assertFalse(visitor.isValid());
    }

    @Test
    public void tesFlat01WithStopField() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215A00F"),
                0, "comName");
        visitor.visit(Flat01Factory.create());
        assertTrue(visitor.isValid());

        visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215A00F"),
                0, "comAmount");
        visitor.visit(Flat01Factory.create());
        assertFalse(visitor.isValid());

    }


    @Test
    public void tesFlat02Valid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E001F0014000F000C"),
                0);
        visitor.visit(Flat02Factory.create());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesFlat02InValid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E0F1F0014000F000C"),
                0);
        visitor.visit(Flat02Factory.create());
        assertFalse(visitor.isValid());
        assertEquals(32, visitor.getLastPos());

    }

    @Test
    public void tesStru01Valid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2"),
                0);
        visitor.visit(Stru01Factory.createStru01Record());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesStru01InValid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F023EC1C2"),
                0);
        visitor.visit(Stru01Factory.createStru01Record());
        assertFalse(visitor.isValid());
        assertEquals(30, visitor.getLastPos());

    }

    @Test
    public void tesStru03Valid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C2000FC1C2000CC1C2"),
                0);
        visitor.visit(Stru03Factory.createStru03Record());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesStru03InValid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C20F0FC1C2000CC1C2"),
                0);
        visitor.visit(Stru03Factory.createStru03Record());
        assertFalse(visitor.isValid());
        assertEquals(42, visitor.getLastPos());

    }

    @Test
    public void tesArdo01Valid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400005000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesArdo01InValidCounterOfOfRange() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400006000000000023556C000000000023656C000000000023756C000000000023856C000000000023956C"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertFalse(visitor.isValid());
        assertEquals(26, visitor.getLastPos());

    }

    @Test
    public void tesArdo01InValidComp3() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400001000000000023F56C"),
                0);
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertFalse(visitor.isValid());
        assertEquals(28, visitor.getLastPos());

    }


    @Test
    public void tesRdef03ValidDefaultStrategyFirstAlternative() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("0001C1C2C3C4404040404040"),
                0);
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesRdef03ValidDefaultStrategySecondAlternative() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("00010250000F"),
                0);
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertTrue(visitor.isValid());

    }

    @Test
    public void tesRdef03ValidCustomStrategy() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("0002F1F2F3F4F50000000000"),
                0, null, new Rdef03ObjectFromHostChoiceStrategy());
        visitor.visit(Rdef03Factory.createRdef03Record());
        assertTrue(visitor.isValid());

    }


    @Test
    public void tesCustdatValid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5C"),
                0);
        visitor.visit(CustdatFactory.createCustomerDataCobolType());
        assertTrue(visitor.isValid());
        assertEquals(108, visitor.getLastPos());

    }

    @Test
    public void tesCustdatInvalid() {

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(cobolContext,
                HexUtils.decodeHex("F0F0F0F0F0F1D1D6C8D540E2D4C9E3C840404040404040404040C3C1D4C2D9C9C4C7C540E4D5C9E5C5D9E2C9E3E8F4F4F0F1F2F5F6F500000002F1F061F0F461F1F1000000000023556C5C5C5C5C5C5C5C5C5CF1F061F0F461F1F100000000002355675C5C5C5C5C5C5C5C5C"),
                0);
        visitor.visit(CustdatFactory.createCustomerDataCobolType());
        assertFalse(visitor.isValid());
        assertEquals(91, visitor.getLastPos());

    }

}
