package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.*;
import com.legstar.converter.type.primitive.*;

public class Flat01RecordFactory {

    public static CobolComplexType create() {
        return createFlat01Record();
    }

    public static CobolComplexType createFlat01Record() {

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

        return new CobolComplexType(fields);

    }

}
