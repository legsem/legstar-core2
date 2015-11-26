package com.legstar.maven.plugin;

import com.legstar.cob2xsd.Cob2XsdConfig;
import org.apache.commons.io.FileUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

/**
 * Properties that are common to all Mojos.
 * 
 */
public abstract class AbstractCoreMojo extends AbstractMojo {

    /**
     * The source directory from which COBOL copybooks are taken. Note that all
     * files within this directory and its sub directories will be considered
     * COBOL copybooks.
     * <p/>
     * Defaults to <b>src/main/cobol</b>.
     * <p/>
     * Note that you can alternatively specify a single file using this
     * parameter.
     */
    @Parameter(property = "sourceDirectory")
    private File sourceDirectory;

    /**
     * Output directory where artifacts will be placed. If the directory does
     * not exist, it will be created.
     * <p/>
     * Defaults to * <b>target/generated-sources</b>.
     */
    @Parameter(property = "outputDirectory")
    private File outputDirectory;

    /**
     * The character encoding for the COBOL copybooks. If this is not specified,
     * the default platform encoding is assumed.
     */
    @Parameter(property = "inputEncoding")
    private String inputEncoding;

    /**
     * An option XSLT transformation to apply on the translated XML schema. This
     * allows a level of customization before the other artifacts are generated.
     */
    @Parameter(property = "xsltFileName")
    private String xsltFileName;

    /**
     * Additional properties.
     */
    @Parameter(property = "configProps")
    private Properties configProps;

    @Parameter(required = true, property = "project")
    protected MavenProject project;
    public void execute() throws MojoExecutionException, MojoFailureException {

        validate();

        if (sourceDirectory.isDirectory()) {
            for (File cobolFile : FileUtils.listFiles(sourceDirectory, null,
                    true)) {
                execute(configProps, cobolFile, inputEncoding, outputDirectory,
                        xsltFileName);
            }
        } else {
            execute(configProps, sourceDirectory, inputEncoding,
                    outputDirectory, xsltFileName);
        }

        project.addCompileSourceRoot( outputDirectory.getAbsolutePath() );
    }

    private void validate() throws MojoFailureException {
        if (sourceDirectory == null) {
            sourceDirectory = new File("src/main/cobol");
        }

        if (!sourceDirectory.exists()) {
            throw new MojoFailureException("Input folder " + sourceDirectory
                    + " does not exist");
        }

        if (outputDirectory == null) {
            outputDirectory = new File("target/generated-sources/"
                    + getDefaultOutputSubDirectory());
        }
        try {
            FileUtils.forceMkdir(outputDirectory);
        } catch (IOException e) {
            throw new MojoFailureException("Unable to create target "
                    + outputDirectory, e);
        }

        if (configProps == null) {
            configProps = Cob2XsdConfig.getDefaultConfigProps();
        }
    }

    public abstract void execute(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output, final String xsltFileName)
            throws MojoExecutionException;

    public abstract String getDefaultOutputSubDirectory();

    public File getSourceDirectory() {
        return sourceDirectory;
    }

    public File getOutputDirectory() {
        return outputDirectory;
    }

    public String getInputEncoding() {
        return inputEncoding;
    }

    public String getXsltFileName() {
        return xsltFileName;
    }

    public Properties getConfigProps() {
        return configProps;
    }

}
