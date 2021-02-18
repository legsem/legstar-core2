package com.legstar.jaxb.generator;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;

public class Cob2JaxbGeneratorMainTest extends AbstractTest {

    private static final boolean CREATE_REFERENCE = false;

    private Cob2JaxbGeneratorMain main;

    private File outputDir;

    @Before
    public void setUp() throws Exception {
        main = new Cob2JaxbGeneratorMain();
        setCreateReferences(CREATE_REFERENCE);
        outputDir = new File("target/test/" + getClass().getSimpleName() + "/"
                + name.getMethodName());
        FileUtils.forceMkdir(outputDir);
    }

    @Test
    public void testGenerateFromSingleFile() {
        try {
            main.execute(new String[] { "-i", TEST_COBOL_FOLDER + "/FLAT01",
                    "-o", outputDir.getAbsolutePath() });
            checkCode("flat01", "Flat01Record");
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    private void checkCode(String subFolder, String recordName)
            throws Exception {
        check(getCode(subFolder, "Cobol" + recordName), "Cobol" + recordName
                + ".java");
        check(getCode(subFolder, recordName), recordName + ".java");
        check(getCode(subFolder, recordName + "Jaxb"), recordName + "Jaxb"
                + ".java");
        check(getCode(subFolder, "Cob2" + recordName + "Converter"), "Cob2"
                + recordName + "Converter" + ".java");
    }

    private String getCode(String subFolder, String className)
            throws IOException {
        return FileUtils.readFileToString(new File(outputDir, subFolder + "/"
                + className + ".java"), StandardCharsets.UTF_8);
    }

}
