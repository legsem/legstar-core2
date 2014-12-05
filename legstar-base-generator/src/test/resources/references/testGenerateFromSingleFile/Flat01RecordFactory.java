

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Flat01RecordFactory {

    public static CobolComplexType create() {
        return createFlat01Record();
    }

    public static CobolComplexType createFlat01Record() {

        final String complexTypeName = "Flat01Record";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > comNumber =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("comNumber", comNumber);

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(20)
                        .build();
        fields.put("comName", comName);

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        return new CobolComplexType(complexTypeName, fields);

    }

}
