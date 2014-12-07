package com.legstar.base.type.primitive;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.utils.HexUtils;

public class CobolFloatTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testFromHostFloat() {
        assertEquals("1234.0", getFloatValue("434d2000"));
        assertEquals("0.0", getFloatValue("00000000"));
        assertEquals("1.0", getFloatValue("41100000"));
        assertEquals("345006.56", getFloatValue("45543ae9"));
        assertEquals("7.982005E-14", getFloatValue("361677a4"));
        assertEquals("3.4028235E38", getFloatValue("60ffffff"));
        assertEquals("25431.121", getFloatValue("4463571f"));
        assertEquals("0.070859075", getFloatValue("401223d2"));
        assertEquals("0.2050727", getFloatValue("40347fa5"));
        assertEquals("0.21563375", getFloatValue("403733c6"));
        assertEquals("0.86346835", getFloatValue("40dd0c43"));
        assertEquals("0.31136322", getFloatValue("404fb580"));
        assertEquals("0.63990897", getFloatValue("40a3d113"));
        assertEquals("0.7430732", getFloatValue("40be3a0c"));
        assertEquals("0.8783575", getFloatValue("40e0dc0a"));
        assertEquals("0.03406465", getFloatValue("3f8b8760"));
        assertEquals("0.99642426", getFloatValue("40ff15a9"));
    }

    @Test
    public void testFromHostNumber() {
        assertEquals("25431.12109375", getValue(Double.class, "4463571f"));
        assertEquals("25431", getValue(Short.class, "4463571f"));
        assertEquals("25431", getValue(Integer.class, "4463571f"));
        assertEquals("25431", getValue(Long.class, "4463571f"));
        assertEquals("25431.12109375", getValue(java.math.BigDecimal.class, "4463571f"));
        assertEquals("25431", getValue(java.math.BigInteger.class, "4463571f"));
        
    }

    private <T extends Number> String getFloatValue(String hexHostData) {
        return getValue(Float.class, hexHostData);
    }

    private <T extends Number> String getValue(Class <T> clazz, String hexHostData) {
        return getType(clazz)
                .fromHost(cobolContext, HexUtils.decodeHex(hexHostData), 0)
                .getValue().toString();
    }
    private <T extends Number> CobolFloatType < T > getType(Class <T> clazz) {
        return new CobolFloatType.Builder < T >(clazz).build();
    }
}
