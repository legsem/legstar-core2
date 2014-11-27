package com.legstar.converter.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;
import com.legstar.converter.type.composite.CobolComplexType;
import com.legstar.converter.type.primitive.CobolBinaryType;
import com.legstar.converter.type.primitive.CobolPackedDecimalType;
import com.legstar.converter.type.primitive.CobolStringType;

public class Rdef02Factory {

    public static CobolComplexType createRdef02Record() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("redf02Key", createRdef02Key());
        children.put("comDetail1Choice", createComDetail1Choice());
        children.put("comItem3",
                new CobolPackedDecimalType.Builder < BigDecimal >(
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());

        return new CobolComplexType(children);
    }

    public static CobolComplexType createRdef02Key() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("redf02Item1Choice", createRedf02Item1Choice());
        children.put("comSelect", new CobolBinaryType.Builder < Short >(
                Short.class).signed(false).totalDigits(4).fractionDigits(0)
                .build());
        return new CobolComplexType(children);
    }

    public static CobolChoiceType createRedf02Item1Choice() {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("comItem1",
                new CobolPackedDecimalType.Builder < BigDecimal >(
                        BigDecimal.class).signed(true).totalDigits(10)
                        .fractionDigits(0).build());
        alternatives.put("comItem2", new CobolStringType.Builder().charNum(6)
                .build());
        return new CobolChoiceType(alternatives);
    }

    public static CobolChoiceType createComDetail1Choice() {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("comDetail1", createComDetail1());
        alternatives.put("comDetail2", createComDetail2());
        return new CobolChoiceType(alternatives);
    }

    public static CobolComplexType createComDetail1() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comName", new CobolStringType.Builder().charNum(10)
                .build());
        return new CobolComplexType(children);
    }

    public static CobolComplexType createComDetail2() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(
                        BigDecimal.class).signed(false).totalDigits(7)
                        .fractionDigits(2).build());
        children.put("filler12", new CobolStringType.Builder().charNum(6)
                .build());
        return new CobolComplexType(children);
    }
}
