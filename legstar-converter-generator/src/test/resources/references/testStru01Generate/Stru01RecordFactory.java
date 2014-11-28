package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.*;
import com.legstar.converter.type.primitive.*;

public class Stru01RecordFactory {

    public static CobolComplexType create() {
        return createStru01Record();
    }

    public static CobolComplexType createComSubRecord() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < java.lang.Short > comItem1 =
                new CobolBinaryType.Builder < java.lang.Short >(java.lang.Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItem1", comItem1);

        CobolStringType comItem2 =
                new CobolStringType.Builder()
                        .charNum(2)
                        .build();
        fields.put("comItem2", comItem2);

        return new CobolComplexType(fields);

    }

    public static CobolComplexType createStru01Record() {

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

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        fields.put("comSubRecord", createComSubRecord());

        return new CobolComplexType(fields);

    }

}
