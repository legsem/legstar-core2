package com.legsem.legstar.finder;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.context.EbcdicCobolContext;
import com.legsem.legstar.type.gen.Flat01Factory;
import com.legsem.legstar.utils.HexUtils;

public class RdwCobolComplexTypeFinderTest {

    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }
    
    @Test
    public void testFlat01() {
        RdwCobolComplexTypeFinder finder = new RdwCobolComplexTypeFinder(Flat01Factory.create(cobolContext), "comNumber");
        
        // No RDW
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        
        // RDW with invalid value (does not correspond to a valid size for flat01)
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("01000000F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 34));
        
        // Valid RDW
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("00220000F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 34));
        
        // Valid RDW but invalid comNumber
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("00220000F0F0FAF0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 34));
    }

}
