package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Stru04RecordFactory {

    public static CobolComplexType create() {
        return createStru04Record();
    }

    public static CobolComplexType createComArray2() {

        final String complexTypeName = "ComArray2";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > ComItem4 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("ComItem4", ComItem4);

        CobolStringType < String > ComArray3 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("ComArray3", new CobolArrayType(ComArray3, 5));

        CobolPackedDecimalType < java.math.BigDecimal > ComItem5 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem5", ComItem5);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComGroup1() {

        final String complexTypeName = "ComGroup1";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > ComItem3 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem3", ComItem3);

        fields.put("ComArray2", new CobolArrayType(createComArray2(), 2));

        CobolBinaryType < Short > ComItem6 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem6", ComItem6);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComArray1() {

        final String complexTypeName = "ComArray1";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > ComItem2 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("ComItem2", ComItem2);

        fields.put("ComGroup1", createComGroup1());

        CobolBinaryType < Integer > ComItem7 =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(8)
                        .build();
        fields.put("ComItem7", ComItem7);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru04Record() {

        final String complexTypeName = "Stru04Record";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > ComItem1 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem1", ComItem1);

        fields.put("ComArray1", new CobolArrayType(createComArray1(), 3));

        CobolPackedDecimalType < java.math.BigDecimal > ComItem8 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("ComItem8", ComItem8);

        return new CobolComplexType(complexTypeName, fields);

    }

}
