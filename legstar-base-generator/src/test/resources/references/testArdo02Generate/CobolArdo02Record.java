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
                    .fields(createArdo02RecordFields())
              );
    }

    private static Map < String, CobolType > createAlternativeAFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > OdoCounter =
                new CobolBinaryType.Builder < Integer >(Integer.class)
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
                        .charNum(2)
                        .build();
        fields.put("Filler8", Filler8);

        return fields;

    }

    private static Map < String, CobolType > createOdoArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > Filler10 =
                new CobolStringType.Builder < String >(String.class)
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

        CobolComplexType OdoArray = new CobolComplexType.Builder()
                        .name("OdoArray")
                        .fields(createOdoArrayFields())
                        .build();
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

        CobolComplexType AlternativeA = new CobolComplexType.Builder()
                        .name("AlternativeA")
                        .fields(createAlternativeAFields())
                        .build();
        fields.put("AlternativeA", AlternativeA);

        CobolComplexType AlternativeB = new CobolComplexType.Builder()
                        .name("AlternativeB")
                        .fields(createAlternativeBFields())
                        .build();
        fields.put("AlternativeB", AlternativeB);

        return fields;

    }

}
