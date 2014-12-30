package com.legstar.cob2xsd;

import java.io.StringReader;
import java.util.Properties;

import org.junit.Before;

public abstract class AbstractTest {

    /** All files from this folder will be tested. */
    public static final String COBOL_SAMPLES_DIR = "src/test/cobol";

    /** Folder holding reference results. */
    public static final String XSD_REFERENCES_DIR = "src/test/references/xsds";

    /** Folder holding test XSLT. */
    public static final String XSLT_SAMPLES_DIR = "src/test/xslt";

    /** Translator options. */
    protected Properties configProps;
    
    protected Cob2XsdConfig defaultConfig;

    @Before
    public void setUp() throws Exception {
        configProps = Cob2XsdConfig.getDefaultConfigProps();
        defaultConfig = new Cob2XsdConfig(configProps);
 
    }

    public String translate(String cobolSource) {
        return translate(cobolSource, "http://www.mycompany.com/test", null);
        
    }

    public String translate(String cobolSource, String xsltFileName) {
        return translate(cobolSource, "http://www.mycompany.com/test", xsltFileName);
        
    }

    public String translate(String cobolSource, String targetNamespace, String xsltFileName) {
        Cob2Xsd cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
        return cob2xsd.translate(new StringReader(cobolSource), targetNamespace, xsltFileName);
        
    }

}
