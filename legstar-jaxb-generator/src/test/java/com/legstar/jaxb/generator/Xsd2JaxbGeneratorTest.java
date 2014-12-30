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
        generateAndCheck("flat01", "Flat01Record");
    }

    @Test
    public void testFlat02Generate() throws Exception {
        generateAndCheck("flat02", "Flat02Record");
    }

    @Test
    public void testStru01Generate() throws Exception {
        generateAndCheck("stru01", "Stru01Record");
    }

    @Test
    public void testStru03Generate() throws Exception {
        generateAndCheck("stru03", "Stru03Record");
    }

    @Test
    public void testRdef01Generate() throws Exception {
        generateAndCheck("rdef01", "Rdef01Record");
    }

    @Test
    public void testRdef02Generate() throws Exception {
        generateAndCheck("rdef02", "Rdef02Record");
    }

    @Test
    public void testRdef03Generate() throws Exception {
        generateAndCheck("rdef03", "Rdef03Record");
    }

    @Test
    public void testArd01Generate() throws Exception {
        generateAndCheck("ardo01", "Ardo01Record");
    }

    @Test
    public void testCustdatGenerate() throws Exception {
        generateAndCheck("custdat", "CustomerData");
    }

    @Test
    public void testStru04Generate() throws Exception {
        generateAndCheck("stru04", "Stru04Record");
    }

    @Test
    public void testCflt01Generate() throws Exception {
        generateAndCheck("cflt01", "Cflt01Record");
    }

    private void generateAndCheck(String schemaName, String recordName)
            throws Exception {
        File xsdFile = new File(TEST_XSD_FOLDER, schemaName + ".xsd");
        Xsd2JaxbGenerator gen = new Xsd2JaxbGenerator();
        Map < String, String > code = gen.generate(xsdFile,
                LEGSTAR_XSD_FILE_ENCODING, "test.example");
        assertEquals(2, code.size());
        check(code.get(getJaxbWrappersFactoryJavaClassName(recordName)), getJaxbWrappersFactoryJavaClassName(recordName) + ".java");
        check(code.get(getJaxbConverterJavaClassName(recordName)), getJaxbConverterJavaClassName(recordName) + ".java");
    }

    private String getJaxbWrappersFactoryJavaClassName(String recordName) {
        return recordName + Xsd2JaxbGenerator.JAXB_WRAPPER_FACTORY_CLASS_NAME_SUFFIX ;
    }

    private String getJaxbConverterJavaClassName(String recordName) {
        return Xsd2JaxbGenerator.JAXB_CONVERTER_CLASS_NAME_PREFIX + recordName;
    }
}
