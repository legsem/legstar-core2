package com.legstar.maven.plugin;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import com.legstar.base.generator.Cob2CobolTypesGenerator;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

/**
 * Goal generates conversion support java classes from a set of COBOL copybooks.
 * 
 */
@Mojo(name = "generate-base", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class BaseGeneratorMojo extends AbstractCoreMojo {

    /**
     * The java package name prefix for generated java classes. The copybook
     * name in lower case forms the last portion of the final package name.
     */
    @Parameter(property = "packageNamePrefix")
    private String packageNamePrefix;

    public void execute(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output, final String xsltFileName)
            throws MojoExecutionException {
        try {
            getLog().info("Processing COBOL file " + cobolFile);
            Cob2CobolTypesGenerator gen = new Cob2CobolTypesGenerator(
                    configProps);
            String baseName = FilenameUtils.getBaseName(
                    cobolFile.getAbsolutePath()).toLowerCase();
            String packageName = packageNamePrefix == null ? baseName
                    : (packageNamePrefix + "." + baseName);

            Map < String, String > code = gen.generate(cobolFile,
                    cobolFileEncoding, packageName, xsltFileName);
            String subFolder = packageName.replace(".", "/") + "/";
            for (Entry < String, String > entry : code.entrySet()) {
                String className = entry.getKey() + ".java";
                getLog().info(
                        "Writing java class " + className + " with package "
                                + packageName + " in " + output);
                FileUtils.writeStringToFile(new File(output, subFolder
                        + className), entry.getValue(), StandardCharsets.UTF_8);
            }
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
