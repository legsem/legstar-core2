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
import com.legstar.base.visitor.MaxBytesLenCobolVisitor;

public class MaxBytesLenCobolVisitorTest {

    @Test
    public void testCalcMaxBytesLenFlat01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Flat01RecordFactory.create());
        assertEquals(30, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenFlat02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Flat02RecordFactory.create());
        assertEquals(40, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenStru03() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Stru03RecordFactory.createStru03Record());
        assertEquals(50, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Rdef01RecordFactory.create());
        assertEquals(12, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Rdef02RecordFactory.create());
        assertEquals(22, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenArdo01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(Ardo01RecordFactory.create());
        assertEquals(68, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(CustomerDataFactory.create());
        assertEquals(183, visitor.getMaxBytesLen());

    }
}
