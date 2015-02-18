package com.legstar.base.type.gen.alltypes;

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

        CobolStringType < String > sString =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("S-STRING")
                        .charNum(4)
                        .build();
        fields.put("sString", sString);

        CobolStringType < java.nio.ByteBuffer > sBinary =
                new CobolStringType.Builder < java.nio.ByteBuffer >(java.nio.ByteBuffer.class)
                        .cobolName("S-BINARY")
                        .charNum(4)
                        .build();
        fields.put("sBinary", sBinary);

        CobolBinaryType < Short > sShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("S-SHORT")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        fields.put("sShort", sShort);

        CobolBinaryType < Integer > sUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("S-USHORT")
                        .totalDigits(4)
                        .build();
        fields.put("sUshort", sUshort);

        CobolBinaryType < Integer > sInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("S-INT")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("sInt", sInt);

        CobolBinaryType < Long > sUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("S-UINT")
                        .totalDigits(9)
                        .build();
        fields.put("sUint", sUint);

        CobolPackedDecimalType < Long > sLong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("S-LONG")
                        .signed(true)
                        .totalDigits(18)
                        .build();
        fields.put("sLong", sLong);

        CobolPackedDecimalType < java.math.BigInteger > sUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-ULONG")
                        .totalDigits(18)
                        .build();
        fields.put("sUlong", sUlong);

        CobolPackedDecimalType < java.math.BigInteger > sXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-XLONG")
                        .signed(true)
                        .totalDigits(31)
                        .build();
        fields.put("sXlong", sXlong);

        CobolPackedDecimalType < java.math.BigInteger > sUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("S-UXLONG")
                        .totalDigits(31)
                        .build();
        fields.put("sUxlong", sUxlong);

        CobolPackedDecimalType < java.math.BigDecimal > sDec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("S-DEC")
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        fields.put("sDec", sDec);

        CobolFloatType < Float > sFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .cobolName("S-FLOAT")
                        .build();
        fields.put("sFloat", sFloat);

        CobolDoubleType < Double > sDouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .cobolName("S-DOUBLE")
                        .build();
        fields.put("sDouble", sDouble);

        CobolStringType < String > aString =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("A-STRING")
                        .charNum(4)
                        .build();
        CobolArrayType aStringArray = new CobolArrayType.Builder()
                        .itemType(aString)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aString", aStringArray);

        CobolStringType < String > aBinary =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("A-BINARY")
                        .charNum(4)
                        .build();
        CobolArrayType aBinaryArray = new CobolArrayType.Builder()
                        .itemType(aBinary)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aBinary", aBinaryArray);

        CobolBinaryType < Short > aShort =
                new CobolBinaryType.Builder < Short >(Short.class)
                        .cobolName("A-SHORT")
                        .signed(true)
                        .totalDigits(4)
                        .build();
        CobolArrayType aShortArray = new CobolArrayType.Builder()
                        .itemType(aShort)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aShort", aShortArray);

        CobolBinaryType < Integer > aUshort =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("A-USHORT")
                        .totalDigits(4)
                        .build();
        CobolArrayType aUshortArray = new CobolArrayType.Builder()
                        .itemType(aUshort)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aUshort", aUshortArray);

        CobolBinaryType < Integer > aInt =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("A-INT")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        CobolArrayType aIntArray = new CobolArrayType.Builder()
                        .itemType(aInt)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aInt", aIntArray);

        CobolBinaryType < Long > aUint =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("A-UINT")
                        .totalDigits(9)
                        .build();
        CobolArrayType aUintArray = new CobolArrayType.Builder()
                        .itemType(aUint)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aUint", aUintArray);

        CobolPackedDecimalType < Long > aLong =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("A-LONG")
                        .signed(true)
                        .totalDigits(18)
                        .build();
        CobolArrayType aLongArray = new CobolArrayType.Builder()
                        .itemType(aLong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aLong", aLongArray);

        CobolPackedDecimalType < java.math.BigInteger > aUlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-ULONG")
                        .totalDigits(18)
                        .build();
        CobolArrayType aUlongArray = new CobolArrayType.Builder()
                        .itemType(aUlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aUlong", aUlongArray);

        CobolPackedDecimalType < java.math.BigInteger > aXlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-XLONG")
                        .signed(true)
                        .totalDigits(31)
                        .build();
        CobolArrayType aXlongArray = new CobolArrayType.Builder()
                        .itemType(aXlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aXlong", aXlongArray);

        CobolPackedDecimalType < java.math.BigInteger > aUxlong =
                new CobolPackedDecimalType.Builder < java.math.BigInteger >(java.math.BigInteger.class)
                        .cobolName("A-UXLONG")
                        .totalDigits(31)
                        .build();
        CobolArrayType aUxlongArray = new CobolArrayType.Builder()
                        .itemType(aUxlong)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aUxlong", aUxlongArray);

        CobolPackedDecimalType < java.math.BigDecimal > aDec =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("A-DEC")
                        .totalDigits(9)
                        .fractionDigits(2)
                        .build();
        CobolArrayType aDecArray = new CobolArrayType.Builder()
                        .itemType(aDec)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aDec", aDecArray);

        CobolFloatType < Float > aFloat =
                new CobolFloatType.Builder < Float >(Float.class)
                        .cobolName("A-FLOAT")
                        .build();
        CobolArrayType aFloatArray = new CobolArrayType.Builder()
                        .itemType(aFloat)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aFloat", aFloatArray);

        CobolDoubleType < Double > aDouble =
                new CobolDoubleType.Builder < Double >(Double.class)
                        .cobolName("A-DOUBLE")
                        .build();
        CobolArrayType aDoubleArray = new CobolArrayType.Builder()
                        .itemType(aDouble)
                        .minOccurs(2)
                        .maxOccurs(2)
                        .build();
        fields.put("aDouble", aDoubleArray);

        return fields;

    }

}
