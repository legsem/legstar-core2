package com.legstar.base.converter;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import com.legstar.base.FromHostResult;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.utils.HexUtils;
import com.legstar.base.visitor.FromCobolChoiceStrategy;

public class Cob2HashMapConverterTest {

    @Test
    public void testConvertFlat01() {
        Cob2HashMapConverter converter = new Cob2HashMapConverter.Builder()
                .cobolComplexType(new com.legstar.base.type.gen.CobolFlat01Record())
                .build();
        FromHostResult < Map <String, Object> > result = converter.convert(
                        HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"));
        assertEquals(30, result.getBytesProcessed());
        assertEquals("{comNumber=1043, comName=NAME000043, comAmount=2150.00}", result.getValue().toString());
    }

    @Test
    public void testConvertRdef01Strategy() {
 
        FromCobolChoiceStrategy customChoiceStrategy = new FromCobolChoiceStrategy() {

            public CobolType choose(String choiceFieldName,
                    CobolChoiceType choiceType,
                    Map < String, Object > variables, byte[] hostData,
                    int start) {
                int select = ((Number) variables.get("comSelect"))
                        .intValue();
                switch (select) {
                case 0:
                    return choiceType.getAlternatives().get(
                            "ComDetail1");
                case 1:
                    return choiceType.getAlternatives().get(
                            "ComDetail2");
                default:
                    return null;
                }
            }

            public Set < String > getVariableNames() {
                Set < String > variables = new HashSet < String >();
                variables.add("comSelect");
                return variables;
            }

        };
        
        Cob2HashMapConverter converter = new Cob2HashMapConverter.Builder()
                .cobolComplexType(new com.legstar.base.type.gen.CobolRdef01Record())
                .customChoiceStrategy(customChoiceStrategy)
                .build();
        FromHostResult < Map <String, Object> > result = converter.convert(
                        HexUtils.decodeHex("00010250000F40404040404000010260000F404040404040"));
        assertEquals(6, result.getBytesProcessed());
        assertEquals("{comSelect=1, comDetail1Choice={comDetail2={comAmount=2500.00}}}", result.getValue().toString());
    }


}
