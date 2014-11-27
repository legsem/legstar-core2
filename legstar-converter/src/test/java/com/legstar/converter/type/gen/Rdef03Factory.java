package com.legstar.converter.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;
import com.legstar.converter.type.composite.CobolComplexType;
import com.legstar.converter.type.primitive.CobolBinaryType;
import com.legstar.converter.type.primitive.CobolPackedDecimalType;
import com.legstar.converter.type.primitive.CobolStringType;
import com.legstar.converter.type.primitive.CobolZonedDecimalType;

public class Rdef03Factory {

    public static CobolComplexType createRdef03Record() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comSelect", new CobolBinaryType.Builder < Short >(
                Short.class).signed(false).totalDigits(4).fractionDigits(0)
                .customVariable(true).build());
        children.put("comDetail1Choice", createComDetail1Choice());
        return new CobolComplexType(children);
    }

    public static CobolChoiceType createComDetail1Choice() {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("comDetail1", createComDetail1());
        alternatives.put("comDetail2", createComDetail2());
        alternatives.put("comDetail3", createComDetail3());
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
        return new CobolComplexType(children);
    }

    public static CobolComplexType createComDetail3() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("comNumber",
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .signed(false).signLeading(false).signSeparate(false)
                        .totalDigits(5).fractionDigits(0).build());
        return new CobolComplexType(children);
    }

}
