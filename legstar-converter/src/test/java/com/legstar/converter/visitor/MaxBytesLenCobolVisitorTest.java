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
import com.legstar.converter.visitor.MaxBytesLenCobolVisitor;

public class MaxBytesLenCobolVisitorTest {

    @Test
    public void testCalcMaxBytesLenFlat01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Flat01Factory.create());
        assertEquals(30, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenFlat02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Flat02Factory.create());
        assertEquals(40, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenStru03() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Stru03Factory.createStru03Record());
        assertEquals(50, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Rdef01Factory.create());
        assertEquals(12, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Rdef02Factory.createRdef02Record());
        assertEquals(22, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenArdo01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Ardo01Factory.createArdo01Record());
        assertEquals(68, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(CustdatFactory.createCustomerDataCobolType());
        assertEquals(183, visitor.getMaxBytesLen());

    }
}
