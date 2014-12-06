package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Stru01RecordFactory {

    public static CobolComplexType create() {
        return createStru01Record();
    }

    public static CobolComplexType createComSubRecord() {

        final String complexTypeName = "ComSubRecord";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem1 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .minInclusive(Short.valueOf("0"))
                        .maxInclusive(Short.valueOf("99"))
                        .build();
        fields.put("comItem1", comItem1);

        CobolStringType < String > comItem2 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(2)
                        .build();
        fields.put("comItem2", comItem2);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru01Record() {

        final String complexTypeName = "Stru01Record";
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

        fields.put("comSubRecord", createComSubRecord());

        return new CobolComplexType(complexTypeName, fields);

    }

}
