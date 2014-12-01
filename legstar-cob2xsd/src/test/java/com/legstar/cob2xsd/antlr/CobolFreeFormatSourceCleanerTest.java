package com.legstar.cob2xsd.antlr;

import static org.junit.Assert.*;

import java.io.StringReader;

import org.junit.Before;
import org.junit.Test;


public class CobolFreeFormatSourceCleanerTest extends AbstractCobolTester {

    /** A shared instance of a fixed format cleaner. */
    private CobolFreeFormatSourceCleaner _cleaner;

    @Before
    public void setUp() throws Exception {
        _cleaner = new CobolFreeFormatSourceCleaner(getErrorHandler());
    }

    /**
     * Comment in first position.
     */
    @Test
    public void testShouldCleanCommentInFirstColumn() throws Exception {
        assertEquals("\n01  DATA.\n",
                _cleaner.clean(new StringReader("* This is a comment\n01  DATA.")));
    }

    /**
     * Comment in some position.
     */
    @Test
    public void testShouldCleanCommentInAnyColumn() throws Exception {
        assertEquals("\n01  DATA.\n",
                _cleaner.clean(new StringReader("     / This is a comment\n01  DATA.")));
    }
}
