package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef02Record extends CobolComplexType {

    public CobolRdef02Record() {
        super("Rdef02Record", createRdef02RecordFields());
    }

    private static Map < String, CobolType > createRdef02KeyFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Item1Choice", new CobolChoiceType("Rdef02Item1Choice",  createRdef02Item1ChoiceFields()));

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        return fields;

    }

    private static Map < String, CobolType > createComDetail1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(10)
                        .build();
        fields.put("comName", comName);

        return fields;

    }

    private static Map < String, CobolType > createComDetail2Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        CobolStringType < String > filler13 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(6)
                        .build();
        fields.put("filler13", filler13);

        return fields;

    }

    private static Map < String, CobolType > createRdef02RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Key", new CobolComplexType("Rdef02Key",  createRdef02KeyFields()));

        fields.put("comDetail1Choice", new CobolChoiceType("ComDetail1Choice",  createComDetail1ChoiceFields()));

        CobolPackedDecimalType < java.math.BigDecimal > comItem3 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
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
                        .signed(true)
                        .totalDigits(10)
                        .build();
        fields.put("rdef02Item1", rdef02Item1);

        CobolStringType < String > rdef02Item2 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(6)
                        .build();
        fields.put("rdef02Item2", rdef02Item2);

        return fields;

    }

    private static Map < String, CobolType > createComDetail1ChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", new CobolComplexType("ComDetail1",  createComDetail1Fields()));

        fields.put("comDetail2", new CobolComplexType("ComDetail2",  createComDetail2Fields()));

        return fields;

    }

}
