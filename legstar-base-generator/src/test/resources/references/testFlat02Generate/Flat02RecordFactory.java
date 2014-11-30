package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Flat02RecordFactory {

    public static CobolComplexType create() {
        return createFlat02Record();
    }

    public static CobolComplexType createFlat02Record() {

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

        CobolBinaryType < java.lang.Short > comArray =
                new CobolBinaryType.Builder < java.lang.Short >(java.lang.Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comArray", new CobolArrayType(comArray, 5));

        return new CobolComplexType(fields);

    }

}
