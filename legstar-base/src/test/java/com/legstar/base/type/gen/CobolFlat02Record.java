package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolFlat02Record extends CobolComplexType {

    public CobolFlat02Record() {
        super(new CobolComplexType.Builder()
                    .name("Flat02Record")
                    .fields(createFlat02RecordFields())
              );
    }

    private static Map < String, CobolType > createFlat02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > comNumber =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("comNumber", comNumber);

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(20)
                        .build();
        fields.put("comName", comName);

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        CobolBinaryType < Short > comArray =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .minInclusive((short) 0)
                        .maxInclusive((short) 99)
                        .build();
        CobolArrayType comArrayArray = new CobolArrayType.Builder()
                        .itemType(comArray)
                        .minOccurs(5)
                        .maxOccurs(5)
                        .build();
        fields.put("comArray", comArrayArray);

        return fields;

    }

}
