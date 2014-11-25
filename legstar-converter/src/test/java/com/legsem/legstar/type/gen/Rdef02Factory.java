package com.legsem.legstar.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolBinaryType;
import com.legsem.legstar.type.primitive.CobolPackedDecimalType;
import com.legsem.legstar.type.primitive.CobolStringType;

public class Rdef02Factory {

    public static CobolComplexType createRdef02Record(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("redf02Key", createRdef02Key(cobolContext));
        children.put("comDetail1Choice", createComDetail1Choice(cobolContext));
        children.put("comItem3",
                new CobolPackedDecimalType.Builder < BigDecimal >(cobolContext,
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());

        return new CobolComplexType(cobolContext, children);
    }

    public static CobolComplexType createRdef02Key(CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("redf02Item1Choice", createRedf02Item1Choice(cobolContext));
        children.put("comSelect", new CobolBinaryType.Builder < Short >(
                cobolContext, Short.class).signed(false).totalDigits(4)
                .fractionDigits(0).build());
        return new CobolComplexType(cobolContext, children);
    }

    public static CobolChoiceType createRedf02Item1Choice(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("comItem1",
                new CobolPackedDecimalType.Builder < BigDecimal >(cobolContext,
                        BigDecimal.class).signed(true).totalDigits(10)
                        .fractionDigits(0).build());
        alternatives.put("comItem2", new CobolStringType.Builder(cobolContext)
                .charNum(6).build());
        return new CobolChoiceType(cobolContext, alternatives);
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
        children.put("filler12", new CobolStringType.Builder(cobolContext)
                .charNum(6).build());
        return new CobolComplexType(cobolContext, children);
    }
}
