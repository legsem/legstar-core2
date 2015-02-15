package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolArdo03Record extends CobolComplexType {

    public CobolArdo03Record() {
        super(new CobolComplexType.Builder()
                    .name("Ardo03Record")
                    .fields(createArdo03RecordFields())
              );
    }

    private static Map < String, CobolType > createOdoSubArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > Filler8 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(4)
                        .build();
        fields.put("Filler8", Filler8);

        return fields;

    }

    private static Map < String, CobolType > createOdoArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Integer > OdoSubCounter =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .totalDigits(3)
                        .minInclusive(Integer.valueOf("0"))
                        .maxInclusive(Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("OdoSubCounter", OdoSubCounter);

        CobolComplexType OdoSubArray = new CobolComplexType.Builder()
                        .name("OdoSubArray")
                        .fields(createOdoSubArrayFields())
                        .build();
        CobolArrayType OdoSubArrayArray = new CobolArrayType.Builder()
                        .itemType(OdoSubArray)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("OdoSubCounter")
                        .build();
        fields.put("OdoSubArray", OdoSubArrayArray);

        return fields;

    }

    private static Map < String, CobolType > createArdo03RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > OdoCounter =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(5)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("OdoCounter", OdoCounter);

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

}
