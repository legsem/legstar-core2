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

        final String complexTypeName = "Rdef02Key";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Item1Choice", createRdef02Item1Choice());

        CobolBinaryType < Integer > comSelect =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("comSelect", comSelect);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComDetail1() {

        final String complexTypeName = "ComDetail1";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(10)
                        .build();
        fields.put("comName", comName);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createComDetail2() {

        final String complexTypeName = "ComDetail2";
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

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createRdef02Record() {

        final String complexTypeName = "Rdef02Record";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("rdef02Key", createRdef02Key());

        fields.put("comDetail1Choice", createComDetail1Choice());

        CobolPackedDecimalType < java.math.BigDecimal > comItem3 =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItem3", comItem3);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolChoiceType createRdef02Item1Choice() {

        final String choiceTypeName = "Rdef02Item1Choice";
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

        return new CobolChoiceType(choiceTypeName, fields);

    }

    public static CobolChoiceType createComDetail1Choice() {

        final String choiceTypeName = "ComDetail1Choice";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("comDetail1", createComDetail1());

        fields.put("comDetail2", createComDetail2());

        return new CobolChoiceType(choiceTypeName, fields);

    }

}