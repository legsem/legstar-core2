package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolStru01Record extends CobolComplexType {

    public CobolStru01Record() {
        super(new CobolComplexType.Builder()
                    .name("Stru01Record")
                    .cobolName("STRU01-RECORD")
                    .fields(createStru01RecordFields())
              );
    }

    private static Map < String, CobolType > createComSubRecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem1 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM1")
                        .signed(true)
                        .totalDigits(4)
                        .minInclusive(Short.valueOf("0"))
                        .maxInclusive(Short.valueOf("99"))
                        .build();
        fields.put("comItem1", comItem1);

        CobolStringType < String > comItem2 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ITEM2")
                        .charNum(2)
                        .build();
        fields.put("comItem2", comItem2);

        return fields;

    }

    private static Map < String, CobolType > createStru01RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > comNumber =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .cobolName("COM-NUMBER")
                        .totalDigits(6)
                        .build();
        fields.put("comNumber", comNumber);

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-NAME")
                        .charNum(20)
                        .build();
        fields.put("comName", comName);

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-AMOUNT")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        CobolComplexType comSubRecord = createComSubRecord();
        fields.put("comSubRecord", comSubRecord);

        return fields;

    }

    public static CobolComplexType createComSubRecord() {

        return new CobolComplexType.Builder()
                .name("ComSubRecord")
                .cobolName("COM-SUB-RECORD")
                .fields(createComSubRecordFields())
                .build();
    }


}

