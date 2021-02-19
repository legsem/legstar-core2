package com.legstar.base.context;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

public class EbcdicCobolContextTest {

    @Test
    public void testInvalidCharset() {
        try {
            new EbcdicCobolContext("INVALID");
            fail();
        } catch (Exception e) {
            assertEquals(
                    "The charset name INVALID is not supported by your java environment."
                            + " You might miss lib/charsets.jar in your jre.",
                    e.getMessage());
        }
    }
}
