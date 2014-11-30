package com.legstar.base.type.primitive;

import static org.junit.Assert.*;

import java.math.BigDecimal;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.primitive.CobolBinaryType;
import com.legstar.base.utils.HexUtils;

public class CobolBinaryTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testIsValidTrueCases() {
        assertTrue(isValid(false, 4, 0, "0000"));
        assertTrue(isValidComp5(false, 4, 0, "79FF"));
    }

    @Test
    public void testIsValidFalseCases() {
        assertFalse(isValid(false, 4, 0, "00"));
        assertFalse(isValidComp5(false, 4, 0, "FF00"));
        assertFalse(isValidComp5(false, 4, 0, "8000"));

    }

    @Test
    public void testFromHostShort() {
        assertEquals("0", getValue(false, 4, 0, "0000"));

        assertEquals("127", getValue(false, 4, 0, "007f"));
        assertEquals("32769", getValueComp5(false, 4, 0, "8001"));
        assertEquals("65535", getValueComp5(false, 4, 0, "ffff"));
        assertEquals("-32768", getValueComp5(true, 4, 0, "8000"));
        assertEquals("-128", getValue(true, 4, 0, "ff80"));
        assertEquals("-75", getValue(true, 4, 0, "ffb5"));
        assertEquals("32767", getValueComp5(true, 4, 0, "7fff"));

    }

    @Test
    public void testFromHostInteger() {
        assertEquals("0", getValue(false, 8, 0, "00000000"));
        assertEquals("74", getValue(false, 8, 0, "0000004a"));
        assertEquals("65534", getValue(false, 8, 0, "0000fffe"));
        assertEquals("2147483649", getValueComp5(false, 8, 0, "80000001"));
        assertEquals("4294967295", getValueComp5(false, 8, 0, "ffffffff"));
        assertEquals("-2147483648", getValueComp5(true, 8, 0, "80000000"));
        assertEquals("-75", getValue(true, 8, 0, "ffffffb5"));
        assertEquals("-128", getValue(true, 8, 0, "ffffff80"));
        assertEquals("123456789", getValueComp5(true, 8, 0, "075bcd15"));
        assertEquals("2147483647", getValueComp5(true, 8, 0, "7fffffff"));

    }

    @Test
    public void testFromHostLong() {
        assertEquals("0", getValue(false, 18, 0, "0000000000000000"));
        assertEquals("74", getValue(false, 18, 0, "000000000000004a"));
        assertEquals("4294967294", getValue(false, 18, 0, "00000000fffffffe"));
        assertEquals("18446744069414584318",
                getValueComp5(false, 18, 0, "fffffffefffffffe"));
        assertEquals("18446744073709551615",
                getValueComp5(false, 18, 0, "ffffffffffffffff"));
        assertEquals("-9223372036854775808",
                getValueComp5(true, 18, 0, "8000000000000000"));
        assertEquals("-75", getValue(true, 18, 0, "ffffffffffffffb5"));
        assertEquals("-4294967294", getValue(true, 18, 0, "ffffffff00000002"));
        assertEquals("17179869183", getValue(true, 18, 0, "00000003ffffffff"));
        assertEquals("9223372036854775807",
                getValueComp5(true, 18, 0, "7fffffffffffffff"));

    }

    @Test
    public void testFromHostShortUnsigned() {
        try {
            getValue(Short.class, false, 4, 0, "FFFF");
            fail();
        } catch (Exception e) {
            assertEquals(
                    "Host unsigned 2 bytes numeric is too large for the target java type java.lang.Short."
                            + " Position is 0." + " Data at position 0x->FFFF",
                    e.getMessage());
        }
        assertEquals("32767", getValueComp5(Short.class, false, 4, 0, "7FFF"));
    }

    @Test
    public void testFromHostShortUnderflow() {
        assertEquals("32767",
                getValueComp5(Short.class, false, 9, 0, "00007FFF"));
        try {
            getValueComp5(Short.class, false, 9, 0, "00008FFF");
            fail();
        } catch (Exception e) {
            assertEquals(
                    "Host unsigned 4 bytes numeric is too large for the target java type java.lang.Short."
                            + " Position is 0."
                            + " Data at position 0x->00008FFF", e.getMessage());
        }
        assertEquals("-1", getValueComp5(Short.class, true, 9, 0, "FFFFFFFF"));
        assertEquals("-1", getValueComp5(Integer.class, true, 9, 0, "FFFFFFFF"));
        assertEquals("-32768",
                getValueComp5(Short.class, true, 9, 0, "FFFF8000"));
        assertEquals("-32768",
                getValueComp5(Integer.class, true, 9, 0, "FFFF8000"));

        try {
            getValueComp5(Short.class, false, 9, 0, "00FF8000");
            fail();
        } catch (Exception e) {
            assertEquals(
                    "Host 4 bytes numeric is too large for the target java type java.lang.Short."
                            + " Position is 0."
                            + " Data at position 0x->00FF8000", e.getMessage());
        }
    }

    @Test
    public void testFromHostDecimal() {
        assertEquals("1234.00", getValue(false, 8, 2, "0001e208"));
        assertEquals("-1234.00", getValue(true, 8, 2, "fffe1df8"));
    }

    private boolean isValid(boolean signed, int totalDigits,
            int fractionDigits, String hexHostData) {

        return getType(BigDecimal.class, signed, totalDigits, fractionDigits)
                .isValid(cobolContext, HexUtils.decodeHex(hexHostData), 0);
    }

    private boolean isValidComp5(boolean signed, int totalDigits,
            int fractionDigits, String hexHostData) {

        return getTypeComp5(BigDecimal.class, signed, totalDigits,
                fractionDigits).isValid(cobolContext,
                HexUtils.decodeHex(hexHostData), 0);
    }

    private String getValue(boolean signed, int totalDigits,
            int fractionDigits, String hexHostData) {
        return getValue(BigDecimal.class, signed, totalDigits, fractionDigits,
                hexHostData);
    }

    private <T extends Number> String getValue(Class < T > clazz,
            boolean signed, int totalDigits, int fractionDigits,
            String hexHostData) {

        return getType(clazz, signed, totalDigits, fractionDigits)
                .fromHost(cobolContext, HexUtils.decodeHex(hexHostData), 0)
                .getValue().toString();

    }

    private <T extends Number> CobolBinaryType < T > getType(Class < T > clazz,
            boolean signed, int totalDigits, int fractionDigits) {
        return new CobolBinaryType.Builder < T >(clazz).signed(signed)
                .totalDigits(totalDigits).fractionDigits(fractionDigits)
                .build();
    }

    private String getValueComp5(boolean signed, int totalDigits,
            int fractionDigits, String hexHostData) {
        return getValueComp5(BigDecimal.class, signed, totalDigits,
                fractionDigits, hexHostData);
    }

    private <T extends Number> String getValueComp5(Class < T > clazz,
            boolean signed, int totalDigits, int fractionDigits,
            String hexHostData) {

        return getTypeComp5(clazz, signed, totalDigits, fractionDigits)
                .fromHost(cobolContext, HexUtils.decodeHex(hexHostData), 0)
                .getValue().toString();

    }

    private <T extends Number> CobolBinaryType < T > getTypeComp5(
            Class < T > clazz, boolean signed, int totalDigits,
            int fractionDigits) {
        return new CobolBinaryType.Builder < T >(clazz).signed(signed)
                .totalDigits(totalDigits).fractionDigits(fractionDigits)
                .minInclusive(null).maxInclusive(null).build();
    }

}
