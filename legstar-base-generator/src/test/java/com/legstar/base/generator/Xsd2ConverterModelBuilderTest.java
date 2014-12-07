package com.legstar.base.generator;

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

import com.legstar.base.generator.Xsd2ConverterModelBuilder;

public class Xsd2ConverterModelBuilderTest extends AbstractTest {

    private static final String LEGSTAR_XSD_FILE_ENCODING = "UTF-8";

    private static final boolean CREATE_REFERENCE = false;

    @Before
    public void setUp() throws Exception {
        setCreateReferences(CREATE_REFERENCE);
    }

    @Test
    public void testFlat01Build() throws Exception {
        check(build("flat01"), "flat01.map");
    }

    @Test
    public void testFlat02Build() throws Exception {
        check(build("flat02"), "flat02.map");
    }

    @Test
    public void testStru01Build() throws Exception {
        check(build("stru01"), "stru01.map");
    }

    @Test
    public void testStru03Build() throws Exception {
        check(build("stru03"), "stru03.map");
    }

    @Test
    public void testArdo01Build() throws Exception {
        check(build("ardo01"), "ardo01.map");
    }

    @Test
    public void testRdef01Build() throws Exception {
        check(build("rdef01"), "rdef01.map");
    }

    @Test
    public void testRdef02Build() throws Exception {
        check(build("rdef02"), "rdef02.map");
    }

    @Test
    public void testRdef03Build() throws Exception {
        check(build("rdef03"), "rdef03.map");
    }

    @Test
    public void testCustdatBuild() throws Exception {
        check(build("custdat"), "custdat.map");
    }

    @Test
    public void testAlltypeBuild() throws Exception {
        check(build("alltypes"), "alltypes.map");
    }

    private String build(String schemaName) throws Exception {
        File xsdFile = new File(TEST_XSD_FOLDER, schemaName + ".xsd");

        Reader reader = new InputStreamReader(new FileInputStream(xsdFile),
                LEGSTAR_XSD_FILE_ENCODING);

        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema xsd = schemaCol.read(new StreamSource(reader));
        Xsd2ConverterModelBuilder builder = new Xsd2ConverterModelBuilder();
        Map < String, Xsd2ConverterModelBuilder.CompositeTypes > model = builder.build(xsd);
        assertEquals(1, model.size());
        return model.toString();
    }

}
