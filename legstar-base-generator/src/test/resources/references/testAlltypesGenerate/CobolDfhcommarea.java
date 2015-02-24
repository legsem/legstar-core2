package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolDfhcommarea extends CobolComplexType {

    public CobolDfhcommarea() {
        super(new CobolComplexType.Builder()
                    .name("Dfhcommarea")
                    .cobolName("DFHCOMMAREA")
                    .fields(createDfhcommareaFields())
              );
    }

    private static Map < String, CobolType > createDfhcommareaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > SString =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("S-STRING")
                        .charNum(4)
                        .build();
        fields.put("SString", SString);

        CobolStringType < java.nio.ByteBuffer > SBinary =
                new CobolStringType.Builder < java.nio.ByteBuffer >(java.nio.ByteBuffer.class)
                        .cobolName("S-BINARY")
                        .charNum(4)
                        .build();
        fields.put("SBinary", SBinary);

        CobolBinaryType < Short > SShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("S-SHORT")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("SShort", SShort);

        CobolBinaryType < Integer > SUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("S-USHORT")
                        .totalDigits(4)
                        .build();
        fields.put("SUshort", SUshort);

        CobolBinaryType < Integer > SInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("S-INT")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("SInt", SInt);

        CobolBinaryType < Long > SUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("S-UINT")
                        .totalDigits(9)
                        .build();
        fields.put("SUint", SUint);

        CobolPackedDecimalType < Long > SLong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("S-LONG")
                        .signed(true)
                        .totalDigits(18)
                        .build();
        fields.put("SLong", SLong);

        CobolPackedDecimalType < java.math.BigInteger > SUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-ULONG")
                        .totalDigits(18)
                        .build();
        fields.put("SUlong", SUlong);

        CobolPackedDecimalType < java.math.BigInteger > SXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-XLONG")
                        .signed(true)
                        .totalDigits(31)
                        .build();
        fields.put("SXlong", SXlong);

        CobolPackedDecimalType < java.math.BigInteger > SUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-UXLONG")
                        .totalDigits(31)
                        .build();
        fields.put("SUxlong", SUxlong);

        CobolPackedDecimalType < java.math.BigDecimal > SDec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("S-DEC")
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        fields.put("SDec", SDec);

        CobolFloatType < Float > SFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .cobolName("S-FLOAT")
                        .build();
        fields.put("SFloat", SFloat);

        CobolDoubleType < Double > SDouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .cobolName("S-DOUBLE")
                        .build();
        fields.put("SDouble", SDouble);

        CobolStringType < String > AString =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("A-STRING")
                        .charNum(4)
                        .build();
        CobolArrayType AStringArray = new CobolArrayType.Builder()
                        .itemType(AString)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AString", AStringArray);

        CobolStringType < java.nio.ByteBuffer > ABinary =
                new CobolStringType.Builder < java.nio.ByteBuffer >(java.nio.ByteBuffer.class)
                        .cobolName("A-BINARY")
                        .charNum(4)
                        .build();
        CobolArrayType ABinaryArray = new CobolArrayType.Builder()
                        .itemType(ABinary)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("ABinary", ABinaryArray);

        CobolBinaryType < Short > AShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("A-SHORT")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        CobolArrayType AShortArray = new CobolArrayType.Builder()
                        .itemType(AShort)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AShort", AShortArray);

        CobolBinaryType < Integer > AUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("A-USHORT")
                        .totalDigits(4)
                        .build();
        CobolArrayType AUshortArray = new CobolArrayType.Builder()
                        .itemType(AUshort)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AUshort", AUshortArray);

        CobolBinaryType < Integer > AInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("A-INT")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        CobolArrayType AIntArray = new CobolArrayType.Builder()
                        .itemType(AInt)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AInt", AIntArray);

        CobolBinaryType < Long > AUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("A-UINT")
                        .totalDigits(9)
                        .build();
        CobolArrayType AUintArray = new CobolArrayType.Builder()
                        .itemType(AUint)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AUint", AUintArray);

        CobolPackedDecimalType < Long > ALong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("A-LONG")
                        .signed(true)
                        .totalDigits(18)
                        .build();
        CobolArrayType ALongArray = new CobolArrayType.Builder()
                        .itemType(ALong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("ALong", ALongArray);

        CobolPackedDecimalType < java.math.BigInteger > AUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-ULONG")
                        .totalDigits(18)
                        .build();
        CobolArrayType AUlongArray = new CobolArrayType.Builder()
                        .itemType(AUlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AUlong", AUlongArray);

        CobolPackedDecimalType < java.math.BigInteger > AXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-XLONG")
                        .signed(true)
                        .totalDigits(31)
                        .build();
        CobolArrayType AXlongArray = new CobolArrayType.Builder()
                        .itemType(AXlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AXlong", AXlongArray);

        CobolPackedDecimalType < java.math.BigInteger > AUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-UXLONG")
                        .totalDigits(31)
                        .build();
        CobolArrayType AUxlongArray = new CobolArrayType.Builder()
                        .itemType(AUxlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AUxlong", AUxlongArray);

        CobolPackedDecimalType < java.math.BigDecimal > ADec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("A-DEC")
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        CobolArrayType ADecArray = new CobolArrayType.Builder()
                        .itemType(ADec)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("ADec", ADecArray);

        CobolFloatType < Float > AFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .cobolName("A-FLOAT")
                        .build();
        CobolArrayType AFloatArray = new CobolArrayType.Builder()
                        .itemType(AFloat)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("AFloat", AFloatArray);

        CobolDoubleType < Double > ADouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .cobolName("A-DOUBLE")
                        .build();
        CobolArrayType ADoubleArray = new CobolArrayType.Builder()
                        .itemType(ADouble)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("ADouble", ADoubleArray);

        return fields;

    }


}

