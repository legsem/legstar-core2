package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolStru04Record extends CobolComplexType {

    public CobolStru04Record() {
        super(new CobolComplexType.Builder()
                    .name("Stru04Record")
                    .cobolName("STRU04-RECORD")
                    .fields(createStru04RecordFields())
              );
    }

    private static Map < String, CobolType > createComArray2Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comItem4 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ITEM4")
                        .charNum(1)
                        .build();
        fields.put("comItem4", comItem4);

        CobolStringType < String > comArray3 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ARRAY3")
                        .charNum(1)
                        .build();
        CobolArrayType comArray3Array = new CobolArrayType.Builder()
                        .itemType(comArray3)
                        .minOccurs(5)
                        .maxOccurs(5)
                        .build();
        fields.put("comArray3", comArray3Array);

        CobolPackedDecimalType < java.math.BigDecimal > comItem5 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM5")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem5", comItem5);

        return fields;

    }

    private static Map < String, CobolType > createComGroup1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem3 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM3")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem3", comItem3);

        CobolComplexType comArray2 = createComArray2();
        CobolArrayType comArray2Array = new CobolArrayType.Builder()
                        .itemType(comArray2)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("comArray2", comArray2Array);

        CobolBinaryType < Short > comItem6 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM6")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem6", comItem6);

        return fields;

    }

    private static Map < String, CobolType > createComArray1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem2 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM2")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem2", comItem2);

        CobolComplexType comGroup1 = createComGroup1();
        fields.put("comGroup1", comGroup1);

        CobolBinaryType < Integer > comItem7 =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("COM-ITEM7")
                        .signed(true)
                        .totalDigits(8)
                        .build();
        fields.put("comItem7", comItem7);

        return fields;

    }

    private static Map < String, CobolType > createStru04RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comItem1 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM1")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem1", comItem1);

        CobolComplexType comArray1 = createComArray1();
        CobolArrayType comArray1Array = new CobolArrayType.Builder()
                        .itemType(comArray1)
                        .minOccurs(3)
                        .maxOccurs(3)
                        .build();
        fields.put("comArray1", comArray1Array);

        CobolPackedDecimalType < java.math.BigDecimal > comItem8 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM8")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem8", comItem8);

        return fields;

    }

    public static CobolComplexType createComArray2() {

        return new CobolComplexType.Builder()
                .name("ComArray2")
                .cobolName("COM-ARRAY2")
                .fields(createComArray2Fields())
                .build();
    }

    public static CobolComplexType createComGroup1() {

        return new CobolComplexType.Builder()
                .name("ComGroup1")
                .cobolName("COM-GROUP1")
                .fields(createComGroup1Fields())
                .build();
    }

    public static CobolComplexType createComArray1() {

        return new CobolComplexType.Builder()
                .name("ComArray1")
                .cobolName("COM-ARRAY1")
                .fields(createComArray1Fields())
                .build();
    }


}

