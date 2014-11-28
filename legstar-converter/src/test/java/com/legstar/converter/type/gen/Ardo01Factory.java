package com.legstar.converter.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolArrayType;
import com.legstar.converter.type.composite.CobolComplexType;
import com.legstar.converter.type.primitive.CobolBinaryType;
import com.legstar.converter.type.primitive.CobolPackedDecimalType;
import com.legstar.converter.type.primitive.CobolStringType;
import com.legstar.converter.type.primitive.CobolZonedDecimalType;

public class Ardo01Factory {

    public static CobolComplexType createArdo01Record() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comNumber",
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .signed(false).signLeading(false).signSeparate(false)
                        .totalDigits(6).fractionDigits(0).build());
        children.put("comName", new CobolStringType.Builder().charNum(20)
                .build());
        children.put(
                "comNbr",
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(false).totalDigits(4).fractionDigits(0)
                        .minInclusive(Short.valueOf("0"))
                        .maxInclusive(Short.valueOf("5"))
                        .odoObject(true)
                        .build());

        CobolPackedDecimalType < BigDecimal > arrayItemType = new CobolPackedDecimalType.Builder < BigDecimal >(
                BigDecimal.class).signed(true).totalDigits(15)
                .fractionDigits(2).build();
        children.put("comArray", new CobolArrayType(arrayItemType, 5, "comNbr"));

        return new CobolComplexType(children);

    }
}
