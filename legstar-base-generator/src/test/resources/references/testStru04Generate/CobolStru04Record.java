package test.example;

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

        CobolStringType < String > ComItem4 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ITEM4")
                        .charNum(1)
                        .build();
        fields.put("ComItem4", ComItem4);

        CobolStringType < String > ComArray3 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ARRAY3")
                        .charNum(1)
                        .build();
        CobolArrayType ComArray3Array = new CobolArrayType.Builder()
                        .itemType(ComArray3)
                        .minOccurs(5)
                        .maxOccurs(5)
                        .build();
        fields.put("ComArray3", ComArray3Array);

        CobolPackedDecimalType < java.math.BigDecimal > ComItem5 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM5")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem5", ComItem5);

        return fields;

    }

    private static Map < String, CobolType > createComGroup1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > ComItem3 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM3")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem3", ComItem3);

        CobolComplexType ComArray2 = createComArray2();
        CobolArrayType ComArray2Array = new CobolArrayType.Builder()
                        .itemType(ComArray2)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("ComArray2", ComArray2Array);

        CobolBinaryType < Short > ComItem6 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM6")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem6", ComItem6);

        return fields;

    }

    private static Map < String, CobolType > createComArray1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > ComItem2 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM2")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem2", ComItem2);

        CobolComplexType ComGroup1 = createComGroup1();
        fields.put("ComGroup1", ComGroup1);

        CobolBinaryType < Integer > ComItem7 =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("COM-ITEM7")
                        .signed(true)
                        .totalDigits(8)
                        .build();
        fields.put("ComItem7", ComItem7);

        return fields;

    }

    private static Map < String, CobolType > createStru04RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > ComItem1 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM1")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem1", ComItem1);

        CobolComplexType ComArray1 = createComArray1();
        CobolArrayType ComArray1Array = new CobolArrayType.Builder()
                        .itemType(ComArray1)
                        .minOccurs(3)
                        .maxOccurs(3)
                        .build();
        fields.put("ComArray1", ComArray1Array);

        CobolPackedDecimalType < java.math.BigDecimal > ComItem8 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM8")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem8", ComItem8);

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

