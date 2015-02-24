package test.example;

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

        CobolBinaryType < Integer > OdoCounter =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("ODO-COUNTER")
                        .totalDigits(4)
                        .minInclusive(Integer.valueOf("0"))
                        .maxInclusive(Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("OdoCounter", OdoCounter);

        return fields;

    }

    private static Map < String, CobolType > createAlternativeBFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > Filler8 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(2)
                        .build();
        fields.put("Filler8", Filler8);

        return fields;

    }

    private static Map < String, CobolType > createOdoArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > Filler10 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(1)
                        .build();
        fields.put("Filler10", Filler10);

        return fields;

    }

    private static Map < String, CobolType > createArdo02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolChoiceType AlternativeAChoice = new CobolChoiceType.Builder()
                        .name("AlternativeAChoice")
                        .alternatives(createAlternativeAChoiceFields())
                        .build();
        fields.put("AlternativeAChoice", AlternativeAChoice);

        CobolComplexType OdoArray = createOdoArray();
        CobolArrayType OdoArrayArray = new CobolArrayType.Builder()
                        .itemType(OdoArray)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("OdoCounter")
                        .build();
        fields.put("OdoArray", OdoArrayArray);

        return fields;

    }

    private static Map < String, CobolType > createAlternativeAChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType AlternativeA = createAlternativeA();
        fields.put("AlternativeA", AlternativeA);

        CobolComplexType AlternativeB = createAlternativeB();
        fields.put("AlternativeB", AlternativeB);

        return fields;

    }
    public static CobolComplexType createOdoArray() {

        return new CobolComplexType.Builder()
                .name("OdoArray")
                .cobolName("ODO-ARRAY")
                .fields(createOdoArrayFields())
                .build();
    }

    public static CobolComplexType createAlternativeA() {

        return new CobolComplexType.Builder()
                .name("AlternativeA")
                .cobolName("ALTERNATIVE-A")
                .fields(createAlternativeAFields())
                .build();
    }

    public static CobolComplexType createAlternativeB() {

        return new CobolComplexType.Builder()
                .name("AlternativeB")
                .cobolName("ALTERNATIVE-B")
                .fields(createAlternativeBFields())
                .build();
    }


}

