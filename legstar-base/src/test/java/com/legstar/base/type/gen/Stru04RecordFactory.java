package com.legstar.base.type.gen;

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

        CobolStringType < String > comItem4 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("comItem4", comItem4);

        CobolStringType < String > comArray3 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("comArray3", new CobolArrayType(comArray3, 5));

        CobolPackedDecimalType < java.math.BigDecimal > comItem5 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem5", comItem5);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComGroup1() {

        final String complexTypeName = "ComGroup1";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem3 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem3", comItem3);

        fields.put("comArray2", new CobolArrayType(createComArray2(), 2));

        CobolBinaryType < Short > comItem6 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem6", comItem6);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComArray1() {

        final String complexTypeName = "ComArray1";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItem2 =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem2", comItem2);

        fields.put("comGroup1", createComGroup1());

        CobolBinaryType < Integer > comItem7 =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(8)
                        .build();
        fields.put("comItem7", comItem7);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru04Record() {

        final String complexTypeName = "Stru04Record";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comItem1 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem1", comItem1);

        fields.put("comArray1", new CobolArrayType(createComArray1(), 3));

        CobolPackedDecimalType < java.math.BigDecimal > comItem8 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem8", comItem8);

        return new CobolComplexType(complexTypeName, fields);

    }

}
