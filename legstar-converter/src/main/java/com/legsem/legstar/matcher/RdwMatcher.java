package com.legsem.legstar.matcher;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolBinaryType;
import com.legsem.legstar.visitor.MaxBytesLenCobolVisitor;
import com.legsem.legstar.visitor.MinBytesLenCobolVisitor;
import com.legsem.legstar.visitor.ValidateFromCobolVisitor;

public class RdwMatcher {

    private final CobolBinaryType<Short> rdw;

    /** Structure that starts with the signature. */
    private final CobolComplexType cobolComplexType;

    /** Where does the signature stop. */
    private final String stopFieldInclusive;

    public RdwMatcher(CobolContext cobolContext,
            CobolComplexType cobolComplexType, String stopFieldInclusive) {

        this.cobolComplexType = cobolComplexType;
        this.stopFieldInclusive = stopFieldInclusive;

        MaxBytesLenCobolVisitor maxLenVisitor = new MaxBytesLenCobolVisitor();
        maxLenVisitor.visit(cobolComplexType);
        MinBytesLenCobolVisitor minLenVisitor = new MinBytesLenCobolVisitor();
        minLenVisitor.visit(cobolComplexType);

        rdw = new CobolBinaryType.Builder < Short >(cobolContext, Short.class)
                .signed(false)
                .totalDigits(4)
                .fractionDigits(0)
                .minInclusive(
                        Short.valueOf((short) minLenVisitor.getMinBytesLen()))
                .maxInclusive(
                        Short.valueOf((short) maxLenVisitor.getMaxBytesLen()))
                .build();
    }

    public boolean doMatch(byte[] hostData, int start, int length) {

        int pos = start;
        if (rdw.isValid(hostData, start)) {
            pos += rdw.getBytesLen();
        } else {
            return false;
        }

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(
                hostData, pos, stopFieldInclusive);
        visitor.visit(cobolComplexType);
        return visitor.isValid();
    }

    public int signatureLen() {
        MaxBytesLenCobolVisitor visitor = new MaxBytesLenCobolVisitor(
                stopFieldInclusive);
        visitor.visit(cobolComplexType);
        return rdw.getBytesLen() + visitor.getMaxBytesLen();
    }

}
