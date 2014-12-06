package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Stru03RecordFactory {

    public static CobolComplexType create() {
        return createStru03Record();
    }

    public static CobolComplexType createComArray() {

        final String complexTypeName = "ComArray";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem1 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .minInclusive((short) 0)
                        .maxInclusive((short) 99)
                        .build();
        fields.put("comItem1", comItem1);

        CobolStringType < String > comItem2 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(2)
                        .build();
        fields.put("comItem2", comItem2);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru03Record() {

        final String complexTypeName = "Stru03Record";
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

        fields.put("comArray", new CobolArrayType(createComArray(), 5));

        return new CobolComplexType(complexTypeName, fields);

    }

}
