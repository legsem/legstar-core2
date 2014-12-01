package com.legstar.base.generator;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.legstar.cob2xsd.Cob2XsdConfig;

public class Cob2ConverterGeneratorTest extends AbstractTest {

    private static final boolean CREATE_REFERENCE = false;

    private static final String LEGSTAR_COBOL_FILE_ENCODING = "UTF-8";

    private Cob2ConverterGenerator gen;

    @Before
    public void setUp() throws Exception {
        gen = new Cob2ConverterGenerator(Cob2XsdConfig.getDefaultConfigProps());
        setCreateReferences(CREATE_REFERENCE);
    }

    @Test
    public void testFlat01Generate() throws Exception {
        check(generate("flat01", "Flat01Record"), "Flat01RecordFactory.java");

    }

    private String generate(String programName, String recordName) {
        File cobolFile = new File(TEST_COBOL_FOLDER, programName);
        Map < String, String > code = gen.generate(cobolFile,
                LEGSTAR_COBOL_FILE_ENCODING, "test.example");
        assertEquals(1, code.size());
        return code.get(recordName + "Factory");
    }
}
