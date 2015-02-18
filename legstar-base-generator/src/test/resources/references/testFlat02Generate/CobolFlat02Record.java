package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolFlat02Record extends CobolComplexType {

    public CobolFlat02Record() {
        super(new CobolComplexType.Builder()
                    .name("Flat02Record")
                    .cobolName("FLAT02-RECORD")
                    .fields(createFlat02RecordFields())
              );
    }

    private static Map < String, CobolType > createFlat02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > comNumber =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .cobolName("COM-NUMBER")
                        .totalDigits(6)
                        .build();
        fields.put("comNumber", comNumber);

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-NAME")
                        .charNum(20)
                        .build();
        fields.put("comName", comName);

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-AMOUNT")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        CobolBinaryType < Short > comArray =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ARRAY")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        CobolArrayType comArrayArray = new CobolArrayType.Builder()
                        .itemType(comArray)
                        .minOccurs(5)
                        .maxOccurs(5)
                        .build();
        fields.put("comArray", comArrayArray);

        return fields;

    }

}
