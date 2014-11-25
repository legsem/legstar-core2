package com.legsem.legstar.visitor;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.context.EbcdicCobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.gen.Rdef01Factory;
import com.legsem.legstar.utils.HexUtils;

public class DefaultFromCobolChoiceStrategyTest {
    
    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testSelectFirstAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy();
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice(cobolContext);
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("0001C1C2C3C440404040404000010260000F404040404040"), 2);
        assertEquals(choice.getAlternatives().get("comDetail1"), alternative);
        
    }

    @Test
    public void testSelectSecondAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy();
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice(cobolContext);
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("00010250000F40404040404000010260000F404040404040"), 2);
        assertEquals(choice.getAlternatives().get("comDetail2"), alternative);
        
    }

    @Test
    public void testFailureToSelectAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy();
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice(cobolContext);
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("000102C2C34040404040404000010260000F404040404040"), 2);
        assertNull(alternative);
        
    }

}
