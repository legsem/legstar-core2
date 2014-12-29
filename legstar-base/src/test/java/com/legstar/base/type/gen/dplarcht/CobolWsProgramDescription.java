package com.legstar.base.type.gen.dplarcht;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolWsProgramDescription extends CobolComplexType {

    public CobolWsProgramDescription() {
        super("WsProgramDescription", createWsProgramDescriptionFields());
    }

    private static Map < String, CobolType > createWsProgramDescriptionFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > wsProgramStart =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsProgramStart", wsProgramStart);

        CobolStringType < String > wsProgramName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsProgramName", wsProgramName);

        CobolBinaryType < Long > wsProgramType =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(8)
                        .build();
        fields.put("wsProgramType", wsProgramType);

        CobolBinaryType < Long > wsProgramLanguage =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(8)
                        .build();
        fields.put("wsProgramLanguage", wsProgramLanguage);

        CobolBinaryType < Integer > wsProgramLength =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("wsProgramLength", wsProgramLength);

        CobolBinaryType < Integer > wsProgramUsecount =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("wsProgramUsecount", wsProgramUsecount);

        return fields;

    }

}
