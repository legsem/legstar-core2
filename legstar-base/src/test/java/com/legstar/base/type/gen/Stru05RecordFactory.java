package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Stru05RecordFactory {

    public static CobolComplexType create() {
        return createStru05Record();
    }

    public static CobolComplexType createComItemA() {

        final String complexTypeName = "ComItemA";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comItemB =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItemB", comItemB);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComItemC() {

        final String complexTypeName = "ComItemC";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItemB =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItemB", comItemB);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComItemE() {

        final String complexTypeName = "ComItemE";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comItemB =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(4)
                        .build();
        fields.put("comItemB", comItemB);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComItemD() {

        final String complexTypeName = "ComItemD";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comItemE", createComItemE());

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru05Record() {

        final String complexTypeName = "Stru05Record";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comItemA", createComItemA());

        fields.put("comItemC", new CobolArrayType(createComItemC(), 3));

        fields.put("comItemD", createComItemD());

        return new CobolComplexType(complexTypeName, fields);

    }

}
