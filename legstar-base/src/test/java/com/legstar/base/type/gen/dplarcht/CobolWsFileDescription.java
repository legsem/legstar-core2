package com.legstar.base.type.gen.dplarcht;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolWsFileDescription extends CobolComplexType {

    public CobolWsFileDescription() {
        super("WsFileDescription", createWsFileDescriptionFields());
    }

    private static Map < String, CobolType > createWsFileDescriptionFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > wsFileStart =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsFileStart", wsFileStart);

        CobolStringType < String > wsFileName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsFileName", wsFileName);

        CobolStringType < String > wsFileDsname =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(44)
                        .build();
        fields.put("wsFileDsname", wsFileDsname);

        CobolBinaryType < Long > wsFileEnablestatus =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(8)
                        .build();
        fields.put("wsFileEnablestatus", wsFileEnablestatus);

        return fields;

    }

}
