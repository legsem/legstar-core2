package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Ardo01RecordFactory {

    public static CobolComplexType create() {
        return createArdo01Record();
    }

    public static CobolComplexType createArdo01Record() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < java.lang.Long > comNumber =
                new CobolZonedDecimalType.Builder < java.lang.Long >(java.lang.Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("comNumber", comNumber);

        CobolStringType comName =
                new CobolStringType.Builder()
                        .charNum(20)
                        .build();
        fields.put("comName", comName);

        CobolBinaryType < java.lang.Integer > comNbr =
                new CobolBinaryType.Builder < java.lang.Integer >(java.lang.Integer.class)
                        .totalDigits(4)
                        .minInclusive(java.lang.Integer.valueOf("0"))
                        .maxInclusive(java.lang.Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("comNbr", comNbr);

        CobolPackedDecimalType < java.math.BigDecimal > comArray =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .signed(true)
                        .totalDigits(15)
                        .fractionDigits(2)
                        .build();
        fields.put("comArray", new CobolArrayType(comArray, 5, "comNbr"));

        return new CobolComplexType(fields);

    }

}
