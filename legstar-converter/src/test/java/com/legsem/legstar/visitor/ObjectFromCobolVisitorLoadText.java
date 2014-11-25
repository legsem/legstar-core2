package com.legsem.legstar.visitor;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;

import org.junit.Before;
import org.junit.Test;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.context.EbcdicCobolContext;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.gen.CustdatFactory;

public class ObjectFromCobolVisitorLoadText {
    
    public static final int RDW_LEN = 4;

    private byte[] rdw = new byte[RDW_LEN];

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testLoad() throws Exception {
        
        MaxBytesLenCobolVisitor macLenCalculator = new MaxBytesLenCobolVisitor();
        macLenCalculator.visit(CustdatFactory.createCustomerDataCobolType(cobolContext));
        byte[] record = new byte[macLenCalculator.getMaxBytesLen()];
        
        CobolComplexType cobolType = CustdatFactory.createCustomerDataCobolType(cobolContext);
        
        FileInputStream is = new FileInputStream(new File("src/test/data/ZOS.FCUSTDAT.RDW.bin"));
        int rdw;
        int count = 0;
        while((rdw = getRecLen(is)) > 0 ) {
            readFully(is, record, 0, rdw);
            ObjectFromCobolVisitor visitor = new ObjectFromCobolVisitor(
                    record,
                    0);
            visitor.visit(cobolType);
            count++;

        }
        assertEquals(10000, count);
    }
    
    private int getRecLen(FileInputStream is) throws Exception {
        int c = is.read(rdw);
        if (c < RDW_LEN) {
            return -1;
        }
        ByteBuffer buf = ByteBuffer.allocate(2);
        buf.put(rdw, 0, 2).position(0);
        return buf.getShort() - RDW_LEN;
        
    }

    public int readFully(InputStream is, byte b[], int off, int len) throws IOException {
        int remaining = len;
        while (remaining > 0) {
            int location = len - remaining;
            int count = is.read(b, off + location, remaining);
            if (-1 == count) { // EOF
                break;
            }
            remaining -= count;
        }
        return len - remaining;
    }

}
