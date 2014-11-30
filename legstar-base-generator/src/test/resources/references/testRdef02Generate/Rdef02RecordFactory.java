package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class Rdef02RecordFactory {

    public static CobolComplexType create() {
        return createRdef02Record();
    }

    public static CobolComplexType createRdef02Key() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Item1Choice", createRdef02Item1Choice());

        CobolBinaryType < java.lang.Integer > comSelect =
                new CobolBinaryType.Builder < java.lang.Integer >(java.lang.Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        return new CobolComplexType(fields);

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

        CobolStringType filler13 =
                new CobolStringType.Builder()
                        .charNum(6)
                        .build();
        fields.put("filler13", filler13);

        return new CobolComplexType(fields);

    }

    public static CobolComplexType createRdef02Record() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Key", createRdef02Key());

        fields.put("comDetail1Choice", createComDetail1Choice());

        CobolPackedDecimalType < java.math.BigDecimal > comItem3 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem3", comItem3);

        return new CobolComplexType(fields);

    }

    public static CobolChoiceType createRdef02Item1Choice() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.lang.Long > rdef02Item1 =
                new CobolPackedDecimalType.Builder < java.lang.Long >(java.lang.Long.class)
                        .signed(true)
                        .totalDigits(10)
                        .build();
        fields.put("rdef02Item1", rdef02Item1);

        CobolStringType rdef02Item2 =
                new CobolStringType.Builder()
                        .charNum(6)
                        .build();
        fields.put("rdef02Item2", rdef02Item2);

        return new CobolChoiceType(fields);

    }

    public static CobolChoiceType createComDetail1Choice() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", createComDetail1());

        fields.put("comDetail2", createComDetail2());

        return new CobolChoiceType(fields);

    }

}
