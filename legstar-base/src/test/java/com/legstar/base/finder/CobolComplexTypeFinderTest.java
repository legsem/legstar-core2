package com.legstar.base.finder;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.finder.CobolComplexTypeFinder;
import com.legstar.base.type.gen.Flat01Factory;
import com.legstar.base.utils.HexUtils;

public class CobolComplexTypeFinderTest {
    
    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testFindFlat01OneField() {
        CobolComplexTypeFinder finder = new CobolComplexTypeFinder(cobolContext, Flat01Factory.create(), "comNumber");
        
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        assertEquals(1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 31));
        assertEquals(3, finder.indexOf(HexUtils.decodeHex("ABF0BCF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 33));
        
        // Buffer too short for a record yet large enough for a signature
        assertEquals(1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F340404040404040404040021500"), 0, 30));
        
        // Part of the comName is confused for a comNumber
        assertEquals(5, finder.indexOf(HexUtils.decodeHex("F0FAF1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        
        // Invalid comName not detected (signature too short)
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F001F0F0F4F3404040404040404040400215000F"), 0, 30));
    }

    @Test
    public void testFindFlat01TwoFields() {
        CobolComplexTypeFinder finder = new CobolComplexTypeFinder(cobolContext, Flat01Factory.create(), "comName");
        
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        assertEquals(1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 31));
        assertEquals(3, finder.indexOf(HexUtils.decodeHex("ABF0BCF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 33));
        assertEquals(1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F340404040404040404040021500"), 0, 30));
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0FAF1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        
        // Now the signature is large enough
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F001F0F0F4F3404040404040404040400215000F"), 0, 30));

        // Invalid comAmount not detected (signature too short)
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F34040404040404040404002A5000F"), 0, 30));

    }

    @Test
    public void testFindFlat01AllFields() {
        CobolComplexTypeFinder finder = new CobolComplexTypeFinder(cobolContext, Flat01Factory.create(), null);
        
        assertEquals(0, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        assertEquals(1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 31));
        assertEquals(3, finder.indexOf(HexUtils.decodeHex("ABF0BCF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 33));
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("ABF0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F340404040404040404040021500"), 0, 30));
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0FAF1F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000F"), 0, 30));
        
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F001F0F0F4F3404040404040404040400215000F"), 0, 30));

        // Now the signature is large enough
        assertEquals(-1, finder.indexOf(HexUtils.decodeHex("F0F0F1F0F4F3D5C1D4C5F0F0F0F0F4F34040404040404040404002A5000F"), 0, 30));

    }


}
