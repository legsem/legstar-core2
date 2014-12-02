package test.example;

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

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createStru03Record() {

        final String complexTypeName = "Stru03Record";
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

        fields.put("comArray", new CobolArrayType(createComArray(), 5));

        return new CobolComplexType(complexTypeName, fields);

    }

}
