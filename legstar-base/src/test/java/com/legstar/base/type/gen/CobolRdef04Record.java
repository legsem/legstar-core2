package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef04Record extends CobolComplexType {

    public CobolRdef04Record() {
        super("Rdef04Record", createRdef04RecordFields());
    }

    private static Map < String, CobolType > createOuterRedefinesShortFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("innerRedefinesLongChoice", new CobolChoiceType("InnerRedefinesLongChoice",  createInnerRedefinesLongChoiceFields()));

        return fields;

    }

    private static Map < String, CobolType > createRdef04RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("outerRedefinesLongChoice", new CobolChoiceType("OuterRedefinesLongChoice",  createOuterRedefinesLongChoiceFields()));

        CobolStringType < String > footer =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("footer", footer);

        return fields;

    }

    private static Map < String, CobolType > createInnerRedefinesLongChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > innerRedefinesLong =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(5)
                        .build();
        fields.put("innerRedefinesLong", innerRedefinesLong);

        CobolStringType < String > innerRedefinesShort =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(3)
                        .build();
        fields.put("innerRedefinesShort", innerRedefinesShort);

        return fields;

    }

    private static Map < String, CobolType > createOuterRedefinesLongChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > outerRedefinesLong =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(10)
                        .build();
        fields.put("outerRedefinesLong", outerRedefinesLong);

        fields.put("outerRedefinesShort", new CobolComplexType("OuterRedefinesShort",  createOuterRedefinesShortFields()));

        return fields;

    }

}
