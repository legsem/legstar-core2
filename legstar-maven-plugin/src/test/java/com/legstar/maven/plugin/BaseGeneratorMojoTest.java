package com.legstar.maven.plugin;

import java.io.File;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;

import com.legstar.maven.plugin.BaseGeneratorMojo;

public class BaseGeneratorMojoTest extends AbstractMojoTestCase {

    protected void setUp() throws Exception {
        super.setUp();
    }

    public void testMojoBasic() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/basic-test-plugin-config.xml");

        BaseGeneratorMojo mojo = (BaseGeneratorMojo) lookupMojo("generate-base", testPom);
        mojo.execute();
        assertTrue(new File(
                "target/generated-sources/java/flat01/CobolFlat01Record.java")
                .exists());
    }

    public void testMojoComplete() throws Exception {
        File testPom = new File(getBasedir(),
                "src/test/resources/poms/complete-test-java-plugin-config.xml");

        BaseGeneratorMojo mojo = (BaseGeneratorMojo) lookupMojo("generate-base", testPom);
        mojo.execute();
        assertTrue(new File(
                "target/generated-test-sources/java/com/example/flat01/CobolFlat01Record.java")
                .exists());
    }

}
