package com.legstar.maven.plugin;

import org.apache.commons.io.FilenameUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import com.legstar.jaxb.generator.Cob2JaxbGenerator;

import java.io.File;
import java.util.Properties;

/**
 * Goal generates conversion support java classes for JAXB from a set of COBOL
 * copybooks.
 * 
 */
@Mojo(name = "generate-jaxb", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class JaxbGeneratorMojo extends AbstractCoreMojo {
 
    /**
     * The java package name prefix for generated java classes. The copybook
     * name in lower case forms the last portion of the final package name.
     */
    @Parameter( property = "packageNamePrefix" )
    private String packageNamePrefix;
    
    public void execute(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output,
            final String xsltFileName) throws MojoExecutionException {
        try {
            getLog().info("Processing COBOL file " + cobolFile);
            Cob2JaxbGenerator gen = new Cob2JaxbGenerator(configProps);
            String baseName = FilenameUtils.getBaseName(
                    cobolFile.getAbsolutePath()).toLowerCase();
            String packageName = packageNamePrefix == null ? baseName
                    : (packageNamePrefix + "." + baseName);

            gen.generate(cobolFile, cobolFileEncoding, output, packageName,
                    xsltFileName);
        } catch (Exception e) {
            throw new MojoExecutionException("Generation failed for "
                    + cobolFile.getAbsolutePath(), e);
        }
    }

    public String getPackageNamePrefix() {
        return packageNamePrefix;
    }

    public String getDefaultOutputSubDirectory() {
        return "java";
    }


}
