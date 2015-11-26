package com.legstar.maven.plugin;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.apache.maven.project.MavenProject;

import java.io.File;

/**
 * @author <a href="mailto:david@davidkarlsen.com">David J. M. Karlsen</a>
 * @since 11/26/15
 */
public abstract class LegStarAbstractMojoTestCase
    extends AbstractMojoTestCase

{
    protected void setUp()
        throws Exception
    {
        super.setUp();
    }

    protected Mojo lookupAndPrepareMojo( String artifactId, File testPom )
        throws Exception
    {
        Mojo mojo = lookupMojo( artifactId, testPom );
        MavenProject mavenProject = new MavenProjectStub();
        setVariableValueToObject( mojo, "project", mavenProject );

        return mojo;
    }
}
