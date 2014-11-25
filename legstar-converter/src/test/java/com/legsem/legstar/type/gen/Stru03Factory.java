package com.legsem.legstar.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.composite.CobolArrayType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolBinaryType;
import com.legsem.legstar.type.primitive.CobolPackedDecimalType;
import com.legsem.legstar.type.primitive.CobolStringType;
import com.legsem.legstar.type.primitive.CobolZonedDecimalType;

public class Stru03Factory {

    public static CobolComplexType createStru03Record(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comNumber",
                new CobolZonedDecimalType.Builder < Integer >(cobolContext,
                        Integer.class).signed(false).signLeading(false)
                        .signSeparate(false).totalDigits(6).fractionDigits(0)
                        .build());
        children.put("comName", new CobolStringType.Builder(cobolContext)
                .charNum(20).build());
        children.put("comAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(cobolContext,
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());

        children.put("comArray", new CobolArrayType(cobolContext,
                createComSubRecord(cobolContext), 5));
        return new CobolComplexType(cobolContext, children);

    }

    public static CobolComplexType createComSubRecord(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comItem1", new CobolBinaryType.Builder < Short >(
                cobolContext, Short.class).signed(true).totalDigits(4)
                .fractionDigits(0).minInclusive(Short.valueOf("0"))
                .maxInclusive(Short.valueOf("99")).build());
        children.put("comItem2", new CobolStringType.Builder(cobolContext)
                .charNum(2).build());
        return new CobolComplexType(cobolContext, children);

    }

}
