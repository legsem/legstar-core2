package com.legstar.jaxb.generator;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;

import com.legstar.cob2xsd.Cob2XsdConfig;

public class Cob2JaxbGeneratorTest extends AbstractTest {

    private static final boolean CREATE_REFERENCE = false;

    private static final String LEGSTAR_COBOL_FILE_ENCODING = "UTF-8";
    
    private static final File OUTPUT_DIR = new File("target/generated-test-sources");
    
    private Cob2JaxbGenerator gen;

    @Before
    public void setUp() throws Exception {
        setCreateReferences(CREATE_REFERENCE);
        gen = new Cob2JaxbGenerator(Cob2XsdConfig.getDefaultConfigProps());
        FileUtils.forceMkdir(OUTPUT_DIR);
        FileUtils.cleanDirectory(OUTPUT_DIR);
    }

    @Test
    public void testFlat01Generate() throws Exception {
        generate("FLAT01");
        check("Flat01Record");
    }


    private void generate(String programName) {
        File cobolFile = new File(TEST_COBOL_FOLDER, programName);
        gen.generate(cobolFile,
                LEGSTAR_COBOL_FILE_ENCODING, "test.example", OUTPUT_DIR);
    }
    
    private void check(String recordName) {
        
        try {
            assertFalse(StringUtils.isBlank(getCode(recordName)));
            assertFalse(StringUtils.isBlank(getCode("Cobol" + recordName)));
            assertFalse(StringUtils.isBlank(getCode(recordName + "Jaxb")));
        } catch (IOException e) {
            fail();
        }
        
    }
    
    private String getCode(String className) throws IOException {
        return FileUtils.readFileToString(new File(OUTPUT_DIR, "test/example/" + className  + ".java"));
    }

}
