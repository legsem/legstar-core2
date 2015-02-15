package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolOptl01Record extends CobolComplexType {

    public CobolOptl01Record() {
        super(new CobolComplexType.Builder()
                    .name("Optl01Record")
                    .fields(createOptl01RecordFields())
              );
    }

    private static Map < String, CobolType > createOptlStructFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > optlStructField1 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(18)
                        .build();
        fields.put("optlStructField1", optlStructField1);

        CobolStringType < String > optlStructField2 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(5)
                        .build();
        fields.put("optlStructField2", optlStructField2);

        return fields;

    }

    private static Map < String, CobolType > createOptl01RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Integer > optlStructInd =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .totalDigits(3)
                        .odoObject(true)
                        .build();
        fields.put("optlStructInd", optlStructInd);

        CobolZonedDecimalType < Integer > optlItemInd =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .totalDigits(3)
                        .odoObject(true)
                        .build();
        fields.put("optlItemInd", optlItemInd);

        CobolComplexType optlStruct = new CobolComplexType.Builder()
                        .name("OptlStruct")
                        .fields(createOptlStructFields())
                        .dependingOn("optlStructInd")
                        .build();
        fields.put("optlStruct", optlStruct);

        CobolStringType < String > optlItem =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(32)
                        .dependingOn("optlItemInd")
                        .build();
        fields.put("optlItem", optlItem);

        return fields;

    }

}
