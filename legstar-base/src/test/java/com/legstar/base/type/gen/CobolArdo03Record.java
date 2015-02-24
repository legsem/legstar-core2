package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolArdo03Record extends CobolComplexType {

    public CobolArdo03Record() {
        super(new CobolComplexType.Builder()
                    .name("Ardo03Record")
                    .cobolName("ARDO03-RECORD")
                    .fields(createArdo03RecordFields())
              );
    }

    private static Map < String, CobolType > createOdoSubArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > filler8 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(4)
                        .build();
        fields.put("filler8", filler8);

        return fields;

    }

    private static Map < String, CobolType > createOdoArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Integer > odoSubCounter =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .cobolName("ODO-SUB-COUNTER")
                        .totalDigits(3)
                        .minInclusive(Integer.valueOf("0"))
                        .maxInclusive(Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("odoSubCounter", odoSubCounter);

        CobolComplexType odoSubArray = createOdoSubArray();
        CobolArrayType odoSubArrayArray = new CobolArrayType.Builder()
                        .itemType(odoSubArray)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("odoSubCounter")
                        .build();
        fields.put("odoSubArray", odoSubArrayArray);

        return fields;

    }

    private static Map < String, CobolType > createArdo03RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > odoCounter =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .cobolName("ODO-COUNTER")
                        .totalDigits(5)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("odoCounter", odoCounter);

        CobolComplexType odoArray = createOdoArray();
        CobolArrayType odoArrayArray = new CobolArrayType.Builder()
                        .itemType(odoArray)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("odoCounter")
                        .build();
        fields.put("odoArray", odoArrayArray);

        return fields;

    }

    public static CobolComplexType createOdoSubArray() {

        return new CobolComplexType.Builder()
                .name("OdoSubArray")
                .cobolName("ODO-SUB-ARRAY")
                .fields(createOdoSubArrayFields())
                .build();
    }

    public static CobolComplexType createOdoArray() {

        return new CobolComplexType.Builder()
                .name("OdoArray")
                .cobolName("ODO-ARRAY")
                .fields(createOdoArrayFields())
                .build();
    }


}

