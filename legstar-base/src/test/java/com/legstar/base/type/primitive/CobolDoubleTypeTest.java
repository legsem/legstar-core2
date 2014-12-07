package com.legstar.base.type.primitive;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.utils.HexUtils;

public class CobolDoubleTypeTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testFromHostDouble() {
        assertEquals("0.0", getDoubleValue("0000000000000000"));
        assertEquals("1.0", getDoubleValue("4110000000000000"));
        assertEquals("345006.56779999996", getDoubleValue("45543ae915b573e8"));
        assertEquals("7.982006699999995E-14", getDoubleValue("361677a4590fab68"));
        assertEquals("3.40282347E38", getDoubleValue("60ffffff048ff9e8"));
        assertEquals("-5.670078E-14", getDoubleValue("b5ff5b8c70649ed0"));
        assertEquals("0.5160421914381664", getDoubleValue("40841b574f9545cd"));
        assertEquals("0.3767408804881589", getDoubleValue("4060721720c34c90"));
        assertEquals("0.5748594107197613", getDoubleValue("409329fc80d6d646"));
        assertEquals("0.6001057816958458", getDoubleValue("4099a0885286303d"));
        assertEquals("0.7850369597199326", getDoubleValue("40c8f82ea425fc38"));
        assertEquals("0.2998016949252067", getDoubleValue("404cbfcdcafd37c0"));
        assertEquals("0.7788391667528523", getDoubleValue("40c76200ee0c21d0"));
        assertEquals("0.8158852356638823", getDoubleValue("40d0dddad4773358"));
        assertEquals("0.4364272923152456", getDoubleValue("406fb9b2f3936868"));
        assertEquals("0.6062248297052384", getDoubleValue("409b318ce99b6f60"));
    }


    @Test
    public void testFromHostNumber() {
        assertEquals("345006.56", getValue(Float.class, "45543ae915b573e8"));
        assertEquals("17326", getValue(Short.class, "45543ae915b573e8"));
        assertEquals("345006", getValue(Integer.class, "45543ae915b573e8"));
        assertEquals("345006", getValue(Long.class, "45543ae915b573e8"));
        assertEquals("345006.567799999960698187351226806640625", getValue(java.math.BigDecimal.class, "45543ae915b573e8"));
        assertEquals("345006", getValue(java.math.BigInteger.class, "45543ae915b573e8"));
        
    }

    private <T extends Number> String getDoubleValue(String hexHostData) {
        return getValue(Double.class, hexHostData);
    }

    private <T extends Number> String getValue(Class <T> clazz, String hexHostData) {
        return getType(clazz)
                .fromHost(cobolContext, HexUtils.decodeHex(hexHostData), 0)
                .getValue().toString();
    }
    private <T extends Number> CobolDoubleType < T > getType(Class <T> clazz) {
        return new CobolDoubleType.Builder < T >(clazz).build();
    }

}
