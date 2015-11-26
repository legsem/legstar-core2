package com.legstar.maven.plugin;

import java.io.File;

public class XsdTranslatorMojoTest extends LegStarAbstractMojoTestCase
{


    public void testMojoBasic() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/basic-test-plugin-config.xml");

        XsdTranslatorMojo mojo = (XsdTranslatorMojo) lookupAndPrepareMojo( "translate-xsd", testPom );
        mojo.execute();
        assertTrue(new File(
                "target/generated-sources/xsds/FLAT01.xsd")
                .exists());
    }

    public void testMojoComplete() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/complete-test-xsd-plugin-config.xml");

        XsdTranslatorMojo mojo = (XsdTranslatorMojo) lookupAndPrepareMojo( "translate-xsd", testPom );
        mojo.execute();
        assertTrue(new File(
                "target/generated-test-sources/xsds/FLAT01.xsd")
                .exists());
    }

}
