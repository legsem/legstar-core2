package com.legstar.base.generator;

import static org.junit.Assert.*;

import java.io.File;

import org.apache.commons.cli.Options;
import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;

public class Cob2ConverterGeneratorMainTest extends AbstractTest {
    
    private static final boolean CREATE_REFERENCE = false;
    
    private Cob2ConverterGeneratorMain main;

    @Before
    public void setUp() throws Exception {
        main = new Cob2ConverterGeneratorMain();
        setCreateReferences(CREATE_REFERENCE);
    }
    private static final File tempFolder = new File(
            System.getProperty("java.io.tmpdir"));
    
    @Test
    public void testProcessCommandLineInvalidOptions() {
        
        Options options = main.createOptions();

        assertFalse(main.collectOptions(options, new String[] {"-toto"}));
        assertFalse(main.collectOptions(options, new String[] {"-i", "zig"}));
        assertFalse(main.collectOptions(options, new String[] {"-o"}));
        assertFalse(main.collectOptions(options, new String[] {"-p", "78"}));
        assertFalse(main.collectOptions(options, new String[] {"-c", "puce"}));
        assertFalse(main.collectOptions(options, new String[] {"-c", "src/test/cobol"}));
        
    }

    @Test
    public void testProcessCommandLineValidOptions() {
        
        Options options = main.createOptions();

        assertTrue(main.collectOptions(options, null));

        assertFalse(main.collectOptions(options,  new String[] {"-v"}));
        assertFalse(main.collectOptions(options,  new String[] {"-h"}));

        assertTrue(main.collectOptions(options, new String[] {"-i", "src/test/cobol"}));
        assertEquals(new File( "src/test/cobol"), main.getInput());
        
        assertTrue(main.collectOptions(options, new String[] {"-i", "src/test/cobol/FLAT01"}));
        assertEquals(new File( "src/test/cobol/FLAT01"), main.getInput());

        assertTrue(main.collectOptions(options, new String[] {"-i", "src/test/cobol", "-enc", "ISO-8859-1"}));
        assertEquals(new File( "src/test/cobol"), main.getInput());
        assertEquals("ISO-8859-1", main.getInputEncoding());
        
        assertTrue(main.collectOptions(options, new String[] {"-o", "target"}));
        assertEquals(new File( "target"), main.getOutput());

        assertTrue(main.collectOptions(options, new String[] {"-o", "target","-p", "test.example"}));
        assertEquals(new File( "target"), main.getOutput());
        assertEquals("test.example", main.getOutputPackage());

        assertTrue(main.collectOptions(options, new String[] {"-c", "src/test/cobol/FLAT01"}));
    }

    @Test
    public void testGenerateFromSingleFile() {
        try {
            main.execute(new String[] {  "-i",
                    TEST_COBOL_FOLDER + "/FLAT01", "-o",
                    tempFolder.getAbsolutePath() });
            File result = new File(tempFolder + "/Flat01RecordFactory.java");
            assertTrue(result.exists());
            check(FileUtils.readFileToString(result), "Flat01RecordFactory.java");
            result.deleteOnExit();
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }


}
