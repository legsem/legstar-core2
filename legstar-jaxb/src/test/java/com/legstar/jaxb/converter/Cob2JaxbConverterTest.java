package com.legstar.jaxb.converter;

import static org.junit.Assert.*;
import legstar.test.jaxb.flat01.CobolFlat01Record;
import legstar.test.jaxb.flat01.Flat01Record;

import org.junit.Test;

import com.legstar.base.converter.FromHostResult;
import com.legstar.base.utils.HexUtils;
import com.legstar.jaxb.converter.gen.flat01.Flat01RecordJaxb;

public class Cob2JaxbConverterTest {

    @Test
    public void testConvertFlat01() {
        Cob2JaxbConverter <Flat01Record> converter = new Cob2JaxbConverter.Builder <Flat01Record>()
                .jaxbClass(Flat01Record.class)
                .jaxbWrapperFactory(new Flat01RecordJaxb())
                .cobolComplexType(new CobolFlat01Record())
                .build();
        FromHostResult < Flat01Record > result = converter.convert(
                        HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"));
        assertEquals(30, result.getBytesProcessed());
        assertEquals("NAME000043", result.getValue().getComName());
        assertEquals(1043, result.getValue().getComNumber());
        assertEquals("2150.00", result.getValue().getComAmount().toString());
    }


}
