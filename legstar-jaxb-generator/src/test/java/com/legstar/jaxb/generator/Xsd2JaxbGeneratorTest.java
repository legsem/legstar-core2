package com.legstar.jaxb.generator;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class Xsd2JaxbGeneratorTest extends AbstractTest {

    private static final boolean CREATE_REFERENCE = false;

    private static final String LEGSTAR_XSD_FILE_ENCODING = "UTF-8";

    @Before
    public void setUp() throws Exception {
        setCreateReferences(CREATE_REFERENCE);
    }

    @Test
    public void testFlat01Generate() throws Exception {
        check(generate("flat01", "Flat01Record"), "Flat01RecordFactory.java");
    }

    @Test
    public void testFlat02Generate() throws Exception {
        check(generate("flat02", "Flat02Record"), "Flat02RecordFactory.java");
    }

    @Test
    public void testStru01Generate() throws Exception {
        check(generate("stru01", "Stru01Record"), "Stru01RecordFactory.java");
    }

    @Test
    public void testStru03Generate() throws Exception {
        check(generate("stru03", "Stru03Record"), "Stru03RecordFactory.java");
    }

    @Test
    public void testRdef01Generate() throws Exception {
        check(generate("rdef01", "Rdef01Record"), "Rdef01RecordFactory.java");
    }

    private String generate(String schemaName, String recordName)
            throws Exception {
        File xsdFile = new File(TEST_XSD_FOLDER, schemaName + ".xsd");
        Xsd2JaxbGenerator gen = new Xsd2JaxbGenerator();
        Map < String, String > code = gen.generate(xsdFile,
                LEGSTAR_XSD_FILE_ENCODING, "test.example");
        assertEquals(1, code.size());
        return code.get(recordName + Xsd2JaxbGenerator.JAVA_CLASS_NAME_SUFFIX);
    }

}
