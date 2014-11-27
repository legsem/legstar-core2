package com.legstar.converter.visitor;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.legstar.converter.type.gen.Ardo01Factory;
import com.legstar.converter.type.gen.CustdatFactory;
import com.legstar.converter.type.gen.Flat01Factory;
import com.legstar.converter.type.gen.Flat02Factory;
import com.legstar.converter.type.gen.Rdef01Factory;
import com.legstar.converter.type.gen.Rdef02Factory;
import com.legstar.converter.type.gen.Stru03Factory;
import com.legstar.converter.visitor.MinBytesLenCobolVisitor;

public class MinBytesLenCobolVisitorTest {

    @Test
    public void testCalcMinBytesLenFlat01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Flat01Factory.create());
        assertEquals(30, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenFlat02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Flat02Factory.create());
        assertEquals(40, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenStru03() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Stru03Factory.createStru03Record());
        assertEquals(50, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Rdef01Factory.create());
        assertEquals(6, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Rdef02Factory.createRdef02Record());
        assertEquals(22, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenArdo01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertEquals(28, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(CustdatFactory.createCustomerDataCobolType());
        assertEquals(58, visitor.getMinBytesLen());

    }

}
