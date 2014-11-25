package com.legsem.legstar.type.primitive;

import static org.junit.Assert.*;

import java.math.BigDecimal;

import org.junit.Before;
import org.junit.Test;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.context.EbcdicCobolContext;
import com.legsem.legstar.type.FromHostException;
import com.legsem.legstar.utils.HexUtils;

public class CobolDecimalTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testDefaultMinMaxInclusive() {
        CobolDecimalType < BigDecimal > cobolDecimal = getCobolDecimal(false,
                5, 0);
        assertEquals("0", cobolDecimal.getMinInclusive().toString());
        assertEquals("99999", cobolDecimal.getMaxInclusive().toString());

        cobolDecimal = getCobolDecimal(true, 5, 0);
        assertEquals("-99999", cobolDecimal.getMinInclusive().toString());
        assertEquals("99999", cobolDecimal.getMaxInclusive().toString());

        cobolDecimal = getCobolDecimal(false, 5, 2);
        assertEquals("0", cobolDecimal.getMinInclusive().toString());
        assertEquals("999.99", cobolDecimal.getMaxInclusive().toString());

        cobolDecimal = getCobolDecimal(true, 5, 2);
        assertEquals("-999.99", cobolDecimal.getMinInclusive().toString());
        assertEquals("999.99", cobolDecimal.getMaxInclusive().toString());

        cobolDecimal = getCobolDecimal(false, 5, 5);
        assertEquals("0", cobolDecimal.getMinInclusive().toString());
        assertEquals("0.99999", cobolDecimal.getMaxInclusive().toString());

        cobolDecimal = getCobolDecimal(true, 5, 5);
        assertEquals("-0.99999", cobolDecimal.getMinInclusive().toString());
        assertEquals("0.99999", cobolDecimal.getMaxInclusive().toString());
    }

    @Test
    public void testDecimalInRange() {
        CobolDecimalType < BigDecimal > cobolDecimal = getCobolDecimal(false,
                5, 0, true, BigDecimal.valueOf(-5), 3);
        assertFalse(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(false, 5, 0, true, BigDecimal.ZERO, 3);
        assertTrue(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(false, 5, 0, true,
                BigDecimal.valueOf(250.35), 3);
        assertTrue(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(false, 5, 0, true,
                BigDecimal.valueOf(99999), 3);
        assertTrue(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(false, 5, 0, true,
                BigDecimal.valueOf(100000), 3);
        assertFalse(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(true, 5, 0, true,
                BigDecimal.valueOf(-250.54), 3);
        assertTrue(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(true, 5, 0, true,
                BigDecimal.valueOf(-99999), 3);
        assertTrue(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));

        cobolDecimal = getCobolDecimal(true, 5, 0, true,
                BigDecimal.valueOf(-100000), 3);
        assertFalse(cobolDecimal.isValid(HexUtils.decodeHex("c1c1c1"), 0));
    }

    public CobolDecimalType < BigDecimal > getCobolDecimal(boolean signed,
            int totalDigits, int fractionDigits) {
        return getCobolDecimal(signed, totalDigits, fractionDigits, true,
                BigDecimal.ZERO, 3);
    }

    public CobolDecimalType < BigDecimal > getCobolDecimal(boolean signed,
            int totalDigits, int fractionDigits, final boolean isValid,
            final BigDecimal value, final int bytesLen) {

        return new MyCobolDecimalType.Builder(cobolContext, BigDecimal.class)
                .signed(signed).totalDigits(totalDigits).fractionDigits(fractionDigits)
                .valid(isValid).fromHost(value).bytesLen(bytesLen)
                .build();
    }

    private static class MyCobolDecimalType extends
            CobolDecimalType < BigDecimal > {

        public static class Builder extends
                CobolDecimalType.Builder < BigDecimal, Builder > {

            private boolean valid;

            private BigDecimal fromHost;

            private int bytesLen;

            public Builder(CobolContext cobolContext, Class < BigDecimal > clazz) {
                super(cobolContext, clazz, 18);
            }

            public Builder valid(boolean value) {
                valid = value;
                return this;
            }

            public Builder fromHost(BigDecimal value) {
                fromHost = value;
                return this;
            }

            public Builder bytesLen(int value) {
                bytesLen = value;
                return this;
            }

            public MyCobolDecimalType build() {
                return new MyCobolDecimalType(this);
            }

            protected Builder self() {
                return this;
            }

        }

        private boolean valid;

        private BigDecimal fromHost;

        private int bytesLen;

        public MyCobolDecimalType(Builder builder) {
            super(builder);
            valid = builder.valid;
            fromHost = builder.fromHost;
            bytesLen = builder.bytesLen;
        }

        protected boolean isValidInternal(Class < BigDecimal > clazz,
                byte[] hostData, int start) {
            return valid;
        }

        protected BigDecimal fromHostInternal(Class < BigDecimal > clazz,
                byte[] hostData, int start) throws FromHostException {
            return fromHost;
        }

        public int getBytesLen() {
            return bytesLen;
        }

    }

}
