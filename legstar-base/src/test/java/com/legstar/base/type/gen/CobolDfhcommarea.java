package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolDfhcommarea extends CobolComplexType {

    public CobolDfhcommarea() {
        super("Dfhcommarea", createDfhcommareaFields());
    }

    private static Map < String, CobolType > createDfhcommareaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > SString =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(4)
                        .build();
        fields.put("SString", SString);

        CobolStringType < java.nio.ByteBuffer > SBinary =
                new CobolStringType.Builder < java.nio.ByteBuffer >(java.nio.ByteBuffer.class)
                        .charNum(4)
                        .build();
        fields.put("SBinary", SBinary);

        CobolBinaryType < Short > SShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("SShort", SShort);

        CobolBinaryType < Integer > SUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("SUshort", SUshort);

        CobolBinaryType < Integer > SInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("SInt", SInt);

        CobolBinaryType < Long > SUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .build();
        fields.put("SUint", SUint);

        CobolPackedDecimalType < Long > SLong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .signed(true)
                        .totalDigits(18)
                        .build();
        fields.put("SLong", SLong);

        CobolPackedDecimalType < java.math.BigInteger > SUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .totalDigits(18)
                        .build();
        fields.put("SUlong", SUlong);

        CobolPackedDecimalType < java.math.BigInteger > SXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .signed(true)
                        .totalDigits(31)
                        .build();
        fields.put("SXlong", SXlong);

        CobolPackedDecimalType < java.math.BigInteger > SUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .totalDigits(31)
                        .build();
        fields.put("SUxlong", SUxlong);

        CobolPackedDecimalType < java.math.BigDecimal > SDec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        fields.put("SDec", SDec);

        CobolFloatType < Float > SFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .build();
        fields.put("SFloat", SFloat);

        CobolDoubleType < Double > SDouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .build();
        fields.put("SDouble", SDouble);

        CobolStringType < String > AString =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(4)
                        .build();
        fields.put("AString", new CobolArrayType(AString, 2));

        CobolStringType < java.nio.ByteBuffer > ABinary =
                new CobolStringType.Builder < java.nio.ByteBuffer >(java.nio.ByteBuffer.class)
                        .charNum(4)
                        .build();
        fields.put("ABinary", new CobolArrayType(ABinary, 2));

        CobolBinaryType < Short > AShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("AShort", new CobolArrayType(AShort, 2));

        CobolBinaryType < Integer > AUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("AUshort", new CobolArrayType(AUshort, 2));

        CobolBinaryType < Integer > AInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("AInt", new CobolArrayType(AInt, 2));

        CobolBinaryType < Long > AUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .build();
        fields.put("AUint", new CobolArrayType(AUint, 2));

        CobolPackedDecimalType < Long > ALong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .signed(true)
                        .totalDigits(18)
                        .build();
        fields.put("ALong", new CobolArrayType(ALong, 2));

        CobolPackedDecimalType < java.math.BigInteger > AUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .totalDigits(18)
                        .build();
        fields.put("AUlong", new CobolArrayType(AUlong, 2));

        CobolPackedDecimalType < java.math.BigInteger > AXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .signed(true)
                        .totalDigits(31)
                        .build();
        fields.put("AXlong", new CobolArrayType(AXlong, 2));

        CobolPackedDecimalType < java.math.BigInteger > AUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .totalDigits(31)
                        .build();
        fields.put("AUxlong", new CobolArrayType(AUxlong, 2));

        CobolPackedDecimalType < java.math.BigDecimal > ADec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        fields.put("ADec", new CobolArrayType(ADec, 2));

        CobolFloatType < Float > AFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .build();
        fields.put("AFloat", new CobolArrayType(AFloat, 2));

        CobolDoubleType < Double > ADouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .build();
        fields.put("ADouble", new CobolArrayType(ADouble, 2));

        return fields;

    }

}
