package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef02Record extends CobolComplexType {

    public CobolRdef02Record() {
        super(new CobolComplexType.Builder()
                    .name("Rdef02Record")
                    .cobolName("RDEF02-RECORD")
                    .fields(createRdef02RecordFields())
              );
    }

    private static Map < String, CobolType > createRdef02KeyFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolChoiceType rdef02Item1Choice = new CobolChoiceType.Builder()
                        .name("Rdef02Item1Choice")
                        .alternatives(createRdef02Item1ChoiceFields())
                        .build();
        fields.put("rdef02Item1Choice", rdef02Item1Choice);

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("COM-SELECT")
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        return fields;

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

        CobolStringType < String > filler13 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(6)
                        .build();
        fields.put("filler13", filler13);

        return fields;

    }

    private static Map < String, CobolType > createRdef02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType rdef02Key = createRdef02Key();
        fields.put("rdef02Key", rdef02Key);

        CobolChoiceType comDetail1Choice = new CobolChoiceType.Builder()
                        .name("ComDetail1Choice")
                        .alternatives(createComDetail1ChoiceFields())
                        .build();
        fields.put("comDetail1Choice", comDetail1Choice);

        CobolPackedDecimalType < java.math.BigDecimal > comItem3 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM3")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem3", comItem3);

        return fields;

    }

    private static Map < String, CobolType > createRdef02Item1ChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < Long > rdef02Item1 =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("RDEF02-ITEM1")
                        .signed(true)
                        .totalDigits(10)
                        .build();
        fields.put("rdef02Item1", rdef02Item1);

        CobolStringType < String > rdef02Item2 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("RDEF02-ITEM2")
                        .charNum(6)
                        .build();
        fields.put("rdef02Item2", rdef02Item2);

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
    public static CobolComplexType createRdef02Key() {

        return new CobolComplexType.Builder()
                .name("Rdef02Key")
                .cobolName("RDEF02-KEY")
                .fields(createRdef02KeyFields())
                .build();
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

