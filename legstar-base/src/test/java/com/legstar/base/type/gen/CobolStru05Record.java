package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolStru05Record extends CobolComplexType {

    public CobolStru05Record() {
        super(new CobolComplexType.Builder()
                    .name("Stru05Record")
                    .cobolName("STRU05-RECORD")
                    .fields(createStru05RecordFields())
              );
    }

    private static Map < String, CobolType > createComItemAFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolPackedDecimalType < java.math.BigDecimal > comItemB =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("COM-ITEM-B")
                        .totalDigits(7)
                        .fractionDigits(2)
                        .build();
        fields.put("comItemB", comItemB);

        return fields;

    }

    private static Map < String, CobolType > createComItemCFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Short > comItemB =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("COM-ITEM-B")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("comItemB", comItemB);

        return fields;

    }

    private static Map < String, CobolType > createComItemEFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > comItemB =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("COM-ITEM-B")
                        .charNum(4)
                        .build();
        fields.put("comItemB", comItemB);

        return fields;

    }

    private static Map < String, CobolType > createComItemDFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType comItemE = createComItemE();
        fields.put("comItemE", comItemE);

        return fields;

    }

    private static Map < String, CobolType > createStru05RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType comItemA = createComItemA();
        fields.put("comItemA", comItemA);

        CobolComplexType comItemC = createComItemC();
        CobolArrayType comItemCArray = new CobolArrayType.Builder()
                        .itemType(comItemC)
                        .minOccurs(3)
                        .maxOccurs(3)
                        .build();
        fields.put("comItemC", comItemCArray);

        CobolComplexType comItemD = createComItemD();
        fields.put("comItemD", comItemD);

        return fields;

    }

    public static CobolComplexType createComItemE() {

        return new CobolComplexType.Builder()
                .name("ComItemE")
                .cobolName("COM-ITEM-E")
                .fields(createComItemEFields())
                .build();
    }

    public static CobolComplexType createComItemA() {

        return new CobolComplexType.Builder()
                .name("ComItemA")
                .cobolName("COM-ITEM-A")
                .fields(createComItemAFields())
                .build();
    }

    public static CobolComplexType createComItemC() {

        return new CobolComplexType.Builder()
                .name("ComItemC")
                .cobolName("COM-ITEM-C")
                .fields(createComItemCFields())
                .build();
    }

    public static CobolComplexType createComItemD() {

        return new CobolComplexType.Builder()
                .name("ComItemD")
                .cobolName("COM-ITEM-D")
                .fields(createComItemDFields())
                .build();
    }


}

