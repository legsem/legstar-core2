package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolArdo01Record extends CobolComplexType {

    public CobolArdo01Record() {
        super(new CobolComplexType.Builder()
                    .name("Ardo01Record")
                    .fields(createArdo01RecordFields())
              );
    }

    private static Map < String, CobolType > createArdo01RecordFields() {

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

        CobolBinaryType < Integer > comNbr =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .minInclusive(Integer.valueOf("0"))
                        .maxInclusive(Integer.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("comNbr", comNbr);

        CobolPackedDecimalType < java.math.BigDecimal > comArray =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .signed(true)
                        .totalDigits(15)
                        .fractionDigits(2)
                        .build();
        CobolArrayType comArrayArray = new CobolArrayType.Builder()
                        .itemType(comArray)
                        .maxOccurs(5)
                        .dependingOn("comNbr")
                        .build();
        fields.put("comArray", comArrayArray);

        return fields;

    }

}
