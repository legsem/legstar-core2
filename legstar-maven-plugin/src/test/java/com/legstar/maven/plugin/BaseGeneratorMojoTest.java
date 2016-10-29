package com.legstar.maven.plugin;

import java.io.File;

public class BaseGeneratorMojoTest extends LegStarAbstractMojoTestCase
{


    public void testMojoBasic() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/basic-test-plugin-config.xml");

        BaseGeneratorMojo mojo = (BaseGeneratorMojo) lookupAndPrepareMojo( "generate-base", testPom );
        mojo.execute();
        assertTrue(new File(
                "target/generated-sources/java/flat01/CobolFlat01Record.java")
                .exists());
    }

    public void testMojoComplete() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/complete-test-java-plugin-config.xml");

        BaseGeneratorMojo mojo = (BaseGeneratorMojo) lookupAndPrepareMojo( "generate-base", testPom );
        mojo.execute();
        assertTrue(new File(
                "target/generated-test-sources/java/com/example/flat01/CobolFlat01Record.java")
                .exists());
    }

}
