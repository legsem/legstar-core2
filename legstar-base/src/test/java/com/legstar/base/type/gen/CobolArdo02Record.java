package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolArdo02Record extends CobolComplexType {

    public CobolArdo02Record() {
        super(new CobolComplexType.Builder()
                    .name("Ardo02Record")
                    .cobolName("ARDO02-RECORD")
                    .fields(createArdo02RecordFields())
              );
    }

    private static Map < String, CobolType > createAlternativeAFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > odoCounter =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("ODO-COUNTER")
                        .totalDigits(4)
                        .minInclusive(Integer.valueOf("0"))
                        .maxInclusive(Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("odoCounter", odoCounter);

        return fields;

    }

    private static Map < String, CobolType > createAlternativeBFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > filler8 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(2)
                        .build();
        fields.put("filler8", filler8);

        return fields;

    }

    private static Map < String, CobolType > createOdoArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > filler10 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(1)
                        .build();
        fields.put("filler10", filler10);

        return fields;

    }

    private static Map < String, CobolType > createArdo02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolChoiceType alternativeAChoice = new CobolChoiceType.Builder()
                        .name("AlternativeAChoice")
                        .alternatives(createAlternativeAChoiceFields())
                        .build();
        fields.put("alternativeAChoice", alternativeAChoice);

        CobolComplexType odoArray = new CobolComplexType.Builder()
                        .name("OdoArray")
                        .cobolName("ODO-ARRAY")
                        .fields(createOdoArrayFields())
                        .build();
        CobolArrayType odoArrayArray = new CobolArrayType.Builder()
                        .itemType(odoArray)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("odoCounter")
                        .build();
        fields.put("odoArray", odoArrayArray);

        return fields;

    }

    private static Map < String, CobolType > createAlternativeAChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType alternativeA = new CobolComplexType.Builder()
                        .name("AlternativeA")
                        .cobolName("ALTERNATIVE-A")
                        .fields(createAlternativeAFields())
                        .build();
        fields.put("alternativeA", alternativeA);

        CobolComplexType alternativeB = new CobolComplexType.Builder()
                        .name("AlternativeB")
                        .cobolName("ALTERNATIVE-B")
                        .fields(createAlternativeBFields())
                        .build();
        fields.put("alternativeB", alternativeB);

        return fields;

    }

}
