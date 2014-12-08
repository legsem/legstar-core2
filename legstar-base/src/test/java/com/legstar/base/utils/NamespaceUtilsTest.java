package com.legstar.base.utils;

import static org.junit.Assert.*;

import org.junit.Test;

public class NamespaceUtilsTest {
    
    @Test
    public void testPackageToNamespace() {
        assertNull(NamespaceUtils.toNamespace(null));
        assertNull(NamespaceUtils.toNamespace(""));
        assertEquals("http://toto", NamespaceUtils.toNamespace("toto"));
        assertEquals("http://toto/tata", NamespaceUtils.toNamespace("toto.tata"));
        assertEquals("http://tata.toto/tutu", NamespaceUtils.toNamespace("toto.tata.tutu"));
    }

}
