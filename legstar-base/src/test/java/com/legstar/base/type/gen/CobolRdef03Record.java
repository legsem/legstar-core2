package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef03Record extends CobolComplexType {

    public CobolRdef03Record() {
        super("Rdef03Record", createRdef03RecordFields());
    }

    private static Map < String, CobolType > createComDetail1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(10)
                        .build();
        fields.put("comName", comName);

        return fields;

    }

    private static Map < String, CobolType > createComDetail2Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        return fields;

    }

    private static Map < String, CobolType > createComDetail3Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > comNumber =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(5)
                        .build();
        fields.put("comNumber", comNumber);

        return fields;

    }

    private static Map < String, CobolType > createRdef03RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .customVariable(true)
                        .build();
        fields.put("comSelect", comSelect);

        fields.put("comDetail1Choice", new CobolChoiceType("ComDetail1Choice",  createComDetail1ChoiceFields()));

        return fields;

    }

    private static Map < String, CobolType > createComDetail1ChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", new CobolComplexType("ComDetail1",  createComDetail1Fields()));

        fields.put("comDetail2", new CobolComplexType("ComDetail2",  createComDetail2Fields()));

        fields.put("comDetail3", new CobolComplexType("ComDetail3",  createComDetail3Fields()));

        return fields;

    }

}
