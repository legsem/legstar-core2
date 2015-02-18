package com.legstar.base.type.primitive;

import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.AsciiCobolContext;
import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.primitive.CobolZonedDecimalType;
import com.legstar.base.utils.HexUtils;

public class CobolZonedDecimalTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testConstructor() {
        try {
            new CobolZonedDecimalType.Builder < BigDecimal >(BigDecimal.class)
                    .cobolName("ZONEDTEST").signed(false).signLeading(true)
                    .signSeparate(true).totalDigits(-1).fractionDigits(-1)
                    .build();
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals("Total digits number -1 cannot be negative",
                    e.getMessage());
        }

        try {
            new CobolZonedDecimalType.Builder < BigDecimal >(BigDecimal.class)
                    .cobolName("ZONEDTEST").signed(false).signLeading(false)
                    .signSeparate(true).totalDigits(0).fractionDigits(-1)
                    .build();
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals("Total digits cannot be zero", e.getMessage());
        }
    }

    @Test
    public void testBytesLength() {
        assertEquals(1, new CobolZonedDecimalType.Builder < BigDecimal >(
                BigDecimal.class).cobolName("ZONEDTEST").signed(false)
                .signLeading(false).signSeparate(false).totalDigits(1)
                .fractionDigits(0).build().getMaxBytesLen());
        assertEquals(2, new CobolZonedDecimalType.Builder < BigDecimal >(
                BigDecimal.class).cobolName("ZONEDTEST").signed(true)
                .signLeading(false).signSeparate(true).totalDigits(1)
                .fractionDigits(0).build().getMaxBytesLen());
        assertEquals(2, new CobolZonedDecimalType.Builder < BigDecimal >(
                BigDecimal.class).cobolName("ZONEDTEST").signed(true)
                .signLeading(true).signSeparate(true).totalDigits(1)
                .fractionDigits(0).build().getMaxBytesLen());
    }

    @Test
    public void testIsValidTrueCases() {
        assertTrue(isValid(false, false, false, 1, 0, "F4"));
        assertTrue(isValid(true, false, false, 1, 0, "C4"));
        assertTrue(isValid(true, false, false, 1, 0, "D4"));
        assertTrue(isValid(true, false, false, 5, 2, "F0F9F1F2C7"));
        assertTrue(isValid(false, false, false, 6, 5, "F0F0F1F2F3F4F5"));
        assertTrue(isValid(false, false, false, 31, 5,
                "F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8F9F0F1"));
        assertTrue(isValid(true, true, true, 8, 6, "60F4F5F9F8F7F5F4F0"));
        assertTrue(isValid(true, true, true, 8, 6, "4EF4F5F9F8F7F5F4F0"));
        assertTrue(isValid(true, true, false, 8, 6, "D4F5F9F8F7F5F4F0"));
        assertTrue(isValid(true, true, false, 8, 6, "C4F5F9F8F7F5F4F0"));
        assertTrue(isValid(true, false, true, 8, 6, "F4F5F9F8F7F5F4F060"));
        assertTrue(isValid(true, false, true, 8, 6, "F4F5F9F8F7F5F4F04E"));
    }

    @Test
    public void testIsValidFalseNonDigitsCases() {
        assertFalse(isValid(false, false, false, 1, 0, "AF"));
        assertFalse(isValid(true, false, false, 5, 2, "0A1C"));
    }

    @Test
    public void testIsValidFalseSignCases() {
        assertFalse(isValid(false, false, false, 1, 0, "C4"));
        assertFalse(isValid(true, false, false, 5, 2, "0310"));
        assertFalse(isValid(true, false, false, 5, 2, "031F"));
        assertFalse(isValid(false, false, false, 5, 2, "031C"));
        assertFalse(isValid(false, false, false, 5, 2, "031D"));
        assertFalse(isValid(true, false, true, 5, 2, "F0F0F9F1F2C7"));
        assertFalse(isValid(true, false, false, 5, 2, "F0F0F9F1F297"));
    }

    @Test
    public void testFromHostDecimal() {
        assertEquals("456790.00675",
                getValue(false, false, false, 11, 5, "f4f5f6f7f9f0f0f0f6f7f5"));
        assertEquals(
                "-123456789012345.12",
                getValue(true, false, false, 17, 2,
                        "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2"));
        assertEquals(
                "0",
                getValue(false, false, false, 17, 0,
                        "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0"));
        assertEquals("-67000.56007",
                getValue(true, true, true, 10, 5, "60f6f7f0f0f0f5f6f0f0f7"));
        assertEquals("-67000.56",
                getValue(true, false, false, 10, 2, "f0f0f0f6f7f0f0f0f5d6"));
        assertEquals("1234567.890",
                getValue(false, false, false, 10, 3, "f1f2f3f4f5f6f7f8f9f0"));
        assertEquals("12", getValue(false, false, false, 5, 0, "f0f0f0f1f2"));
        assertEquals(
                "1234567890123456789012345678901",
                getValue(false, false, false, 31, 0,
                        "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1"));
        assertEquals("-59.875404",
                getValue(true, true, false, 8, 6, "d4f5f9f8f7f5f4f0"));
        assertEquals("-45.987540",
                getValue(true, false, true, 8, 6, "f4f5f9f8f7f5f4f060"));
        assertEquals("-45.987540",
                getValue(true, false, false, 8, 6, "f4f5f9f8f7f5f4d0"));
    }

    @Test
    public void testFromHostDecimalSpecialCases() {
        assertEquals("0", getValue(false, false, false, 2, 0, "40F0"));
        assertEquals("0", getValue(false, false, false, 2, 0, "0000"));
        assertEquals("-5", getValue(true, false, false, 1, 0, "D5"));
        assertEquals("5", getValue(true, false, false, 1, 0, "F5"));
    }

    @Test
    public void testFromHostDecimalAscii() {
        cobolContext = new AsciiCobolContext();
        assertEquals(
                "-123456789012345.12",
                getValue(true, true, true, 17, 2,
                        "2d3132333435363738393031323334353132"));
        assertEquals(
                "-1234567890123.45",
                getValue(true, true, true, 17, 2,
                        "2d2020313233343536373839303132333435313220"));
    }

    @Test
    public void testFromHostDecimalVariousJavaTypes() {
        FromHostPrimitiveResult < Short > result = getResult(Short.class, false, false, false, 11, 5,
                "f4f5f6f7f9f0f0f0f6f7f5");
        assertFalse(result.isSuccess());
        assertEquals("Host 11 bytes numeric converts to '456790.00675' which is not a valid java.lang.Short."
                + " Error at offset 0 : [0xF4F5F6F7F9F0F0F0F6F7F5]", result.getErrorMessage());
        assertEquals(
                "456790",
                getValue(Integer.class, false, false, false, 11, 5,
                        "f4f5f6f7f9f0f0f0f6f7f5"));
        assertEquals(
                "456790",
                getValue(Long.class, false, false, false, 11, 5,
                        "f4f5f6f7f9f0f0f0f6f7f5"));
        assertEquals(
                "456790",
                getValue(BigInteger.class, false, false, false, 11, 5,
                        "f4f5f6f7f9f0f0f0f6f7f5"));
        assertEquals(
                "456790.00675",
                getValue(BigDecimal.class, false, false, false, 11, 5,
                        "f4f5f6f7f9f0f0f0f6f7f5"));

    }

    private boolean isValid(boolean signed, boolean signLeading,
            boolean signSeparate, int totalDigits, int fractionDigits,
            String hexHostData) {

        CobolZonedDecimalType < BigDecimal > type = new CobolZonedDecimalType.Builder < BigDecimal >(
                BigDecimal.class).cobolName("ZONEDTEST").signed(signed)
                .signLeading(signLeading).signSeparate(signSeparate)
                .totalDigits(totalDigits).fractionDigits(fractionDigits)
                .build();

        return type.isValid(cobolContext, HexUtils.decodeHex(hexHostData), 0);
    }

    private String getValue(boolean signed, boolean signLeading,
            boolean signSeparate, int totalDigits, int fractionDigits,
            String hexHostData) {
        return getValue(BigDecimal.class, signed, signLeading, signSeparate,
                totalDigits, fractionDigits, hexHostData);
    }

    private <T extends Number> String getValue(Class < T > clazz,
            boolean signed, boolean signLeading, boolean signSeparate,
            int totalDigits, int fractionDigits, String hexHostData) {
        return getResult(clazz, signed, signLeading, signSeparate, totalDigits,
                fractionDigits, hexHostData).getValue().toString();

    }

    private <T extends Number> FromHostPrimitiveResult < T > getResult(
            Class < T > clazz, boolean signed, boolean signLeading,
            boolean signSeparate, int totalDigits, int fractionDigits,
            String hexHostData) {

        CobolZonedDecimalType < T > type = new CobolZonedDecimalType.Builder < T >(
                clazz).cobolName("ZONEDTEST").signed(signed)
                .signLeading(signLeading).signSeparate(signSeparate)
                .totalDigits(totalDigits).fractionDigits(fractionDigits)
                .build();

        return type.fromHost(cobolContext, HexUtils.decodeHex(hexHostData), 0);

    }
}
