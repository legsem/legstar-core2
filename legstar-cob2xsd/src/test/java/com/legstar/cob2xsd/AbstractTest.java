package com.legstar.cob2xsd;

import java.io.File;
import java.io.StringReader;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.junit.Before;

public abstract class AbstractTest {

    /** All files from this folder will be tested. */
    public static final String COBOL_SAMPLES_DIR = "src/test/cobol";

    /** Folder holding reference results. */
    public static final String XSD_REFERENCES_DIR = "src/test/references/xsds";

    /** Folder holding test XSLT. */
    public static final String XSLT_SAMPLES_DIR = "src/test/xslt";

    /** Where generated schemas are stored (cleanable location). */
    public static final String GEN_XSD_DIR = "target/gen/schema";

    /** Translator options. */
    protected Properties configProps;
    
    protected Cob2XsdConfig defaultConfig;

    protected File xsdGenDir;

    @Before
    public void setUp() throws Exception {
        configProps = Cob2XsdConfig.getDefaultConfigProps();
        defaultConfig = new Cob2XsdConfig(configProps);
        
        xsdGenDir = new File(GEN_XSD_DIR);
        FileUtils.deleteQuietly(xsdGenDir);
        FileUtils.forceMkdir(xsdGenDir);
 
    }

    public String translate(String cobolSource) {
        Cob2Xsd cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
        return cob2xsd.translate(new StringReader(cobolSource));
        
    }

}
