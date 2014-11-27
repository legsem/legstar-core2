package com.legstar.converter.visitor;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.legstar.converter.context.CobolContext;
import com.legstar.converter.context.EbcdicCobolContext;
import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;
import com.legstar.converter.type.gen.Rdef01Factory;
import com.legstar.converter.utils.HexUtils;
import com.legstar.converter.visitor.DefaultFromCobolChoiceStrategy;

public class DefaultFromCobolChoiceStrategyTest {
    
    private CobolContext cobolContext;

    @Before
    public void setUp() {
        cobolContext = new EbcdicCobolContext();
    }

    @Test
    public void testSelectFirstAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy(cobolContext);
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice();
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("0001C1C2C3C440404040404000010260000F404040404040"), 2);
        assertEquals(choice.getAlternatives().get("comDetail1"), alternative);
        
    }

    @Test
    public void testSelectSecondAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy(cobolContext);
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice();
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("00010250000F40404040404000010260000F404040404040"), 2);
        assertEquals(choice.getAlternatives().get("comDetail2"), alternative);
        
    }

    @Test
    public void testFailureToSelectAlternative() {
        DefaultFromCobolChoiceStrategy strategy = new DefaultFromCobolChoiceStrategy(cobolContext);
        CobolChoiceType choice = Rdef01Factory.createComDetail1Choice();
        CobolType alternative = strategy.choose("comDetail1Choice", choice, null, HexUtils.decodeHex("000102C2C34040404040404000010260000F404040404040"), 2);
        assertNull(alternative);
        
    }

}
