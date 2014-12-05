package com.legstar.base.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPackedDecimalType;
import com.legstar.base.type.primitive.CobolStringType;
import com.legstar.base.type.primitive.CobolZonedDecimalType;

public class Flat01Factory {

    public static CobolComplexType create() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comNumber",
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .signed(false).signLeading(false).signSeparate(false)
                        .totalDigits(6).fractionDigits(0).build());
        children.put("comName", new CobolStringType.Builder <String>(String.class)
                .charNum(20)
                .build());
        children.put("comAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());
        return new CobolComplexType("Flat01", children);

    }
}
