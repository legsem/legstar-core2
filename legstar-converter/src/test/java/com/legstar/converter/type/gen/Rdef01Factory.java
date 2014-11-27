package com.legstar.converter.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.converter.context.CobolContext;
import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;
import com.legstar.converter.type.composite.CobolComplexType;
import com.legstar.converter.type.primitive.CobolBinaryType;
import com.legstar.converter.type.primitive.CobolPackedDecimalType;
import com.legstar.converter.type.primitive.CobolStringType;

public class Rdef01Factory {

    public static CobolComplexType create(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comSelect", new CobolBinaryType.Builder < Short >(
                cobolContext, Short.class).customVariable(true).signed(false)
                .totalDigits(4).fractionDigits(0).customVariable(true).build());
        children.put("comDetail1Choice", createComDetail1Choice(cobolContext));
        return new CobolComplexType(cobolContext, children);
    }

    public static CobolChoiceType createComDetail1Choice(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("comDetail1", createComDetail1(cobolContext));
        alternatives.put("comDetail2", createComDetail2(cobolContext));
        return new CobolChoiceType(cobolContext, alternatives);
    }

    public static CobolComplexType createComDetail1(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comName", new CobolStringType.Builder(cobolContext)
                .charNum(10).build());
        return new CobolComplexType(cobolContext, children);
    }

    public static CobolComplexType createComDetail2(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(cobolContext,
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());
        return new CobolComplexType(cobolContext, children);
    }
}
