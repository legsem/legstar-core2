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
import com.legstar.base.visitor.MinBytesLenCobolVisitor;

public class MinBytesLenCobolVisitorTest {

    @Test
    public void testCalcMinBytesLenFlat01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolFlat01Record());
        assertEquals(30, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenFlat02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolFlat02Record());
        assertEquals(40, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenStru03() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolStru03Record());
        assertEquals(50, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolRdef01Record());
        assertEquals(6, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenRdef02() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolRdef02Record());
        assertEquals(22, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMinBytesLenArdo01() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolArdo01Record());
        assertEquals(28, visitor.getMinBytesLen());

    }

    @Test
    public void testCalcMaxBytesLenCustdat() {
        MinBytesLenCobolVisitor visitor = new MinBytesLenCobolVisitor();
        visitor.visit(new CobolCustomerData());
        assertEquals(58, visitor.getMinBytesLen());

    }

}
