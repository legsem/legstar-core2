package com.legstar.base.visitor;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.legstar.base.type.gen.CobolArdo01Record;
import com.legstar.base.type.gen.CobolCustomerData;
import com.legstar.base.type.gen.CobolFlat01Record;
import com.legstar.base.type.gen.CobolFlat02Record;
import com.legstar.base.type.gen.CobolRdef01Record;
import com.legstar.base.type.gen.CobolRdef02Record;
import com.legstar.base.type.gen.CobolStru03Record;
import com.legstar.base.visitor.MaxBytesLenCobolVisitor;

public class MaxBytesLenCobolVisitorTest {

    @Test
    public void testCalcMaxBytesLenFlat01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolFlat01Record());
        assertEquals(30, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenFlat02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolFlat02Record());
        assertEquals(40, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenStru03() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolStru03Record());
        assertEquals(50, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolRdef01Record());
        assertEquals(12, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenRdef02() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolRdef02Record());
        assertEquals(22, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenArdo01() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolArdo01Record());
        assertEquals(68, visitor.getMaxBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor();
        visitor.visit(new CobolCustomerData());
        assertEquals(183, visitor.getMaxBytesLen());

    }
}
