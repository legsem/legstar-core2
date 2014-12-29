package com.legstar.base.type.gen.dplarcht;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolWsTransactionDescription extends CobolComplexType {

    public CobolWsTransactionDescription() {
        super("WsTransactionDescription", createWsTransactionDescriptionFields());
    }

    private static Map < String, CobolType > createWsTransactionDescriptionFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > wsTransactionStart =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsTransactionStart", wsTransactionStart);

        CobolStringType < String > wsTransactionName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsTransactionName", wsTransactionName);

        CobolStringType < String > wsTransactionProgram =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("wsTransactionProgram", wsTransactionProgram);

        CobolBinaryType < Long > wsTransactionStatus =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(8)
                        .build();
        fields.put("wsTransactionStatus", wsTransactionStatus);

        return fields;

    }

}
