package com.legstar.converter.generator;


import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Map;

import javax.xml.transform.stream.StreamSource;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.junit.Before;
import org.junit.Test;

public class CobXsd2ConverterTest extends AbstractTest {

    private static final String LEGSTAR_XSD_FILE_ENCODING = "UTF-8";

    private static final boolean CREATE_REFERENCE = false;

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
    public void testArdo01Generate() throws Exception {
        check(generate("ardo01", "Ardo01Record"), "Ardo01RecordFactory.java");
    }
    
    @Test
    public void testRdef01Generate() throws Exception {
        check(generate("rdef01", "Rdef01Record"), "Rdef01RecordFactory.java");
    }
    
    @Test
    public void testRdef02Generate() throws Exception {
        check(generate("rdef02", "Rdef02Record"), "Rdef02RecordFactory.java");
    }
    
    @Test
    public void testRdef03Generate() throws Exception {
        check(generate("rdef03", "Rdef03Record"), "Rdef03RecordFactory.java");
    }
    
    @Test
    public void testCustdatGenerate() throws Exception {
        check(generate("custdat", "CustomerData"), "CustomerDataFactory.java");
    }
    
    private String generate(String schemaName, String recordName) throws Exception {
        File xsdFile = new File(TEST_XSD_FOLDER, schemaName + ".xsd");

        Reader reader = new InputStreamReader(new FileInputStream(xsdFile),
                LEGSTAR_XSD_FILE_ENCODING);

        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema xsd = schemaCol.read(new StreamSource(reader));
        CobXsd2Converter gen = new CobXsd2Converter();
        Map <String, String> code = gen.generate(xsd, "test.example");
        assertEquals(1, code.size());
        return code.get(recordName + "Factory");
    }

}
