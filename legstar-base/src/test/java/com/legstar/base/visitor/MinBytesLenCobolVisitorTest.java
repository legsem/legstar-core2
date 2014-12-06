package com.legstar.base.visitor;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.legstar.base.type.gen.Ardo01RecordFactory;
import com.legstar.base.type.gen.CustomerDataFactory;
import com.legstar.base.type.gen.Flat01RecordFactory;
import com.legstar.base.type.gen.Flat02RecordFactory;
import com.legstar.base.type.gen.Rdef01RecordFactory;
import com.legstar.base.type.gen.Rdef02RecordFactory;
import com.legstar.base.type.gen.Stru03RecordFactory;
import com.legstar.base.visitor.MinBytesLenCobolVisitor;

public class MinBytesLenCobolVisitorTest {

    @Test
    public void testCalcMinBytesLenFlat01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Flat01RecordFactory.create());
        assertEquals(30, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenFlat02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Flat02RecordFactory.create());
        assertEquals(40, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenStru03() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Stru03RecordFactory.createStru03Record());
        assertEquals(50, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Rdef01RecordFactory.create());
        assertEquals(6, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Rdef02RecordFactory.create());
        assertEquals(22, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenArdo01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(Ardo01RecordFactory.create());
        assertEquals(28, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(CustomerDataFactory.create());
        assertEquals(58, visitor.getMinBytesLen());

    }

}
