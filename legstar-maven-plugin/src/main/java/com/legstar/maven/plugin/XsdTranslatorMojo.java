package com.legstar.maven.plugin;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import com.legstar.cob2xsd.Cob2XsdConfig;
import com.legstar.cob2xsd.Cob2XsdIO;
import com.legstar.cob2xsd.antlr.RecognizerException;

import java.io.File;
import java.util.Properties;

/**
 * Goal translates a set of COBOL copybooks into XML schemas.
 * 
 */
@Mojo(name = "translate-xsd", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class XsdTranslatorMojo extends AbstractCoreMojo {

    /**
     * The XML schema target namespace is formed using this prefix and the
     * copybook name in lower case.
     */
    @Parameter(property = "targetNamespacePrefix")
    private String targetNamespacePrefix;

    /**
     * Translates a single COBOL source file.
     * 
     * @param configProps configuration parameters
     * @param cobolFile COBOL source file
     * @param cobolFileEncoding the COBOL file character encoding
     * @param target target file or folder
     * @param targetNamespacePrefix the output XML schemas target namespace
     *            prefix
     * @param xsltFileName an optional xslt to apply on the XML Schema
     * @throws MojoExecutionException if parser fails
     */
    public void execute(Properties configProps, final File cobolFile,
            final String cobolFileEncoding, final File target,
            final String xsltFileName) throws MojoExecutionException {
        try {
            getLog().info("Processing COBOL file " + cobolFile);
            Cob2XsdIO cob2XsdIO = new Cob2XsdIO(new Cob2XsdConfig(configProps));
            File xsdFile = cob2XsdIO.translate(cobolFile, cobolFileEncoding, target,
                    targetNamespacePrefix, xsltFileName);
            getLog().info("COBOL copybook translated to XML schema " + xsdFile);
        } catch (RecognizerException e) {
            throw new MojoExecutionException("Translation failed for "
                    + cobolFile.getAbsolutePath(), e);
        }
    }

    public String getTargetNamespacePrefix() {
        return targetNamespacePrefix;
    }

    public String getDefaultOutputSubDirectory() {
        return "xsds";
    }

}
