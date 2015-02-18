package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolCflt01Record extends CobolComplexType {

    public CobolCflt01Record() {
        super(new CobolComplexType.Builder()
                    .name("Cflt01Record")
                    .cobolName("CFLT01-RECORD")
                    .fields(createCflt01RecordFields())
              );
    }

    private static Map < String, CobolType > createCfltInfo9Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > cfltId =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CFLT-ID")
                        .charNum(18)
                        .build();
        fields.put("cfltId", cfltId);

        CobolStringType < String > cfltTypCd =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CFLT-TYP-CD")
                        .charNum(5)
                        .build();
        fields.put("cfltTypCd", cfltTypCd);

        return fields;

    }

    private static Map < String, CobolType > createCfltParent1Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType cfltInfo = new CobolComplexType.Builder()
                        .name("CfltInfo9")
                        .cobolName("CFLT-INFO")
                        .fields(createCfltInfo9Fields())
                        .build();
        fields.put("cfltInfo", cfltInfo);

        return fields;

    }

    private static Map < String, CobolType > createCfltInfo13Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > cfltIdCt =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CFLT-ID-CT")
                        .charNum(18)
                        .build();
        fields.put("cfltIdCt", cfltIdCt);

        CobolStringType < String > cfltTypCdCt =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CFLT-TYP-CD-CT")
                        .charNum(5)
                        .build();
        fields.put("cfltTypCdCt", cfltTypCdCt);

        return fields;

    }

    private static Map < String, CobolType > createCfltParent2Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType cfltInfo = new CobolComplexType.Builder()
                        .name("CfltInfo13")
                        .cobolName("CFLT-INFO")
                        .fields(createCfltInfo13Fields())
                        .build();
        fields.put("cfltInfo", cfltInfo);

        return fields;

    }

    private static Map < String, CobolType > createCflt01RecordFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType cfltParent1 = new CobolComplexType.Builder()
                        .name("CfltParent1")
                        .cobolName("CFLT-PARENT1")
                        .fields(createCfltParent1Fields())
                        .build();
        fields.put("cfltParent1", cfltParent1);

        CobolComplexType cfltParent2 = new CobolComplexType.Builder()
                        .name("CfltParent2")
                        .cobolName("CFLT-PARENT2")
                        .fields(createCfltParent2Fields())
                        .build();
        fields.put("cfltParent2", cfltParent2);

        return fields;

    }

}
