package com.legstar.converter.type.primitive;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.legstar.converter.context.AsciiCobolContext;
import com.legstar.converter.context.CobolContext;
import com.legstar.converter.context.EbcdicCobolContext;
import com.legstar.converter.type.primitive.CobolStringType;
import com.legstar.converter.utils.HexUtils;

public class CobolStringTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testConstructor() {
        try {
            new CobolStringType.Builder(cobolContext).charNum(-1).build();
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals("Characters number -1 cannot be negative",
                    e.getMessage());
        }
        try {
            new CobolStringType.Builder(cobolContext).charNum(Integer.MAX_VALUE).build();
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "Characters number 2147483647 is greater than maximum 16777215",
                    e.getMessage());
        }
    }

    @Test
    public void testIsValidTrueCases() {
        assertTrue(isValid(1, "00"));
        assertTrue(isValid(1, "40"));
        assertTrue(isValid(1, "FF"));

        assertTrue(isValid(2, "4000"));
        assertTrue(isValid(2, "4040"));
        assertTrue(isValid(2, "40FF"));
    }

    @Test
    public void testIsValidFalseCases() {
        assertFalse(isValid(1, "01"));
        assertFalse(isValid(1, "39"));
        assertFalse(isValid(2, "4001"));
        assertFalse(isValid(2, "4039"));

    }

    @Test
    public void testFromHostTrailStripping() {
        assertEquals("ABCD", getValue(4, "c1c2c3c4"));
        assertEquals("  CD", getValue(6, "4040c3c44040"));
        assertEquals("  CD", getValue(6, "4040c3c40000"));
        assertEquals("", getValue(6, "404040404040"));
        assertEquals("", getValue(6, "000000000000"));
    }

    @Test
    public void testFromHostTrailStrippingNoSpace() {
        cobolContext = new NoTrailingSpaceTrimmingContext();
        assertEquals("  CD  ", getValue(6, "4040c3c44040"));
    }

    private class NoTrailingSpaceTrimmingContext extends EbcdicCobolContext {

        @Override
        public boolean isTruncateHostStringsTrailingSpaces() {
            return false;
        }

    }

    @Test
    public void testFromHostTrailStrippingAscii() {
        cobolContext = new AsciiCobolContext();
        assertEquals("@@@", getValue(3, "404040"));
    }

    @Test
    public void testFromHostTruncated() {
        assertEquals("ABCD", getValue(4, "c1c2c3c4"));
    }

    @Test
    public void testFromHostInnerLowValues() {
        assertEquals("A CD", getValue(8, "c100c3c4"));
    }

    @Test
    public void testFromHostLatin1() {
        cobolContext = new AsciiCobolContext();
        assertEquals("  ABCD", getValue(6, "202041424344"));
    }

    @Test
    public void testFromHostFrenchEbcdic() {
        cobolContext = new EbcdicCobolContext("IBM01147");
        assertEquals(
                "ça c'est un problème élémentaire à résoudre",
                getValue(
                        43,
                        "e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985"));
    }

   private boolean isValid(int charNum, String hexHostData) {
        return new CobolStringType.Builder(cobolContext).charNum(charNum).build().isValid(
                HexUtils.decodeHex(hexHostData), 0);
    }

    private String getValue(int charNum, String hexHostData) {
        return new CobolStringType.Builder(cobolContext).charNum(charNum).build().fromHost(
                HexUtils.decodeHex(hexHostData), 0).getValue();

    }

}
