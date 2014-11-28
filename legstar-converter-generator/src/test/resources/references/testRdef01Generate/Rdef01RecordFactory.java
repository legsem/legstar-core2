package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.*;
import com.legstar.converter.type.primitive.*;

public class Rdef01RecordFactory {

    public static CobolComplexType create() {
        return createRdef01Record();
    }

    public static CobolComplexType createComDetail1() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType comName =
                new CobolStringType.Builder()
                        .charNum(10)
                        .build();
        fields.put("comName", comName);

        return new CobolComplexType(fields);

    }

    public static CobolComplexType createComDetail2() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comAmount", comAmount);

        return new CobolComplexType(fields);

    }

    public static CobolComplexType createRdef01Record() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < java.lang.Integer > comSelect =
                new CobolBinaryType.Builder < java.lang.Integer >(java.lang.Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        fields.put("comDetail1Choice", createComDetail1Choice());

        return new CobolComplexType(fields);

    }

    public static CobolChoiceType createComDetail1Choice() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", createComDetail1());

        fields.put("comDetail2", createComDetail2());

        return new CobolChoiceType(fields);

    }

}
