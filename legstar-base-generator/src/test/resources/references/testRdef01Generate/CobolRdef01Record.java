package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolRdef01Record extends CobolComplexType {

    public CobolRdef01Record() {
        super("Rdef01Record", createRdef01RecordFields());
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

        return fields;

    }

    private static Map < String, CobolType > createRdef01RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        fields.put("comDetail1Choice", new CobolChoiceType("ComDetail1Choice",  createComDetail1ChoiceFields()));

        return fields;

    }

    private static Map < String, CobolType > createComDetail1ChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", new CobolComplexType("ComDetail1",  createComDetail1Fields()));

        fields.put("comDetail2", new CobolComplexType("ComDetail2",  createComDetail2Fields()));

        return fields;

    }

}
