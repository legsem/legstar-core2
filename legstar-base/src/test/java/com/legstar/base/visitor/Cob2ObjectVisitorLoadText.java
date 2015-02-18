package com.legstar.base.visitor;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.gen.CobolCustomerData;
import com.legstar.base.visitor.Cob2ObjectVisitor;

public class Cob2ObjectVisitorLoadText {

    public static final int RDW_LEN = 4;

    private byte[] rdw = new byte[RDW_LEN];

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testLoad() throws Exception {

        CobolComplexType cobolType = new CobolCustomerData();
        byte[] record = new byte[cobolType.getMaxBytesLen() > cobolContext
                .getMaxCobolTypeBytesLen() ? cobolContext
                .getMaxCobolTypeBytesLen() : (int) cobolType.getMaxBytesLen()];

        FileInputStream is = new FileInputStream(new File(
                "src/test/data/ZOS.FCUSTDAT.RDW.bin"));
        int rdw;
        int count = 0;
        while ((rdw = getRecLen(is)) > 0) {
            readFully(is, record, 0, rdw);
            Cob2ObjectVisitor visitor = new Cob2ObjectVisitor(cobolContext,
                    record, 0);
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

    public int readFully(InputStream is, byte b[], int off, int len)
            throws IOException {
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
