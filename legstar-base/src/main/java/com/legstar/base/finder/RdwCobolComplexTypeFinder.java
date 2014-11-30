package com.legstar.base.finder;

import java.nio.ByteBuffer;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.composite.CobolComplexType;

/**
 * When complex types are preceded by a Record Descriptor Word (RDW), the RDW
 * itself can be used as part of the matching pattern.
 *
 */
public class RdwCobolComplexTypeFinder extends CobolComplexTypeFinder {

    public static final int RDW_LEN = 4;

    public RdwCobolComplexTypeFinder(CobolContext cobolContext,
            CobolComplexType cobolComplexType, String stopFieldInclusive) {
        super(cobolContext, cobolComplexType, stopFieldInclusive);

    }

    public boolean match(byte[] hostData, int start, int length) {

        int reclen = getRdw(hostData, start, length) - RDW_LEN;
        if (reclen < getMinBytesLen() || reclen > getMaxBytesLen()) {
            return false;
        }
        return super.match(hostData, start + RDW_LEN, length);
    }

    public int getRdw(byte[] hostData, int start, int length) {
        if (start + RDW_LEN > length) {
            return -1; // not enough bytes for an RDW
        }
        ByteBuffer buf = ByteBuffer.allocate(2);
        buf.put(hostData, start, 2).position(0);
        return buf.getShort();
    }

}
