package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef01Record extends CobolComplexType {

    public CobolRdef01Record() {
        super(new CobolComplexType.Builder()
                    .name("Rdef01Record")
                    .cobolName("RDEF01-RECORD")
                    .fields(createRdef01RecordFields())
              );
    }

    private static Map < String, CobolType > createComDetail1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-NAME")
                        .charNum(10)
                        .build();
        fields.put("comName", comName);

        return fields;

    }

    private static Map < String, CobolType > createComDetail2Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-AMOUNT")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        return fields;

    }

    private static Map < String, CobolType > createRdef01RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("COM-SELECT")
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        CobolChoiceType comDetail1Choice = new CobolChoiceType.Builder()
                        .name("ComDetail1Choice")
                        .alternatives(createComDetail1ChoiceFields())
                        .build();
        fields.put("comDetail1Choice", comDetail1Choice);

        return fields;

    }

    private static Map < String, CobolType > createComDetail1ChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType comDetail1 = createComDetail1();
        fields.put("comDetail1", comDetail1);

        CobolComplexType comDetail2 = createComDetail2();
        fields.put("comDetail2", comDetail2);

        return fields;

    }
    public static CobolComplexType createComDetail1() {

        return new CobolComplexType.Builder()
                .name("ComDetail1")
                .cobolName("COM-DETAIL1")
                .fields(createComDetail1Fields())
                .build();
    }

    public static CobolComplexType createComDetail2() {

        return new CobolComplexType.Builder()
                .name("ComDetail2")
                .cobolName("COM-DETAIL2")
                .fields(createComDetail2Fields())
                .build();
    }


}

