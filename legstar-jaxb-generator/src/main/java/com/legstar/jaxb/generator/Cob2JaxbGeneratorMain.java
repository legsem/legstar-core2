package com.legstar.jaxb.generator;

import java.io.File;
import java.util.Properties;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.base.generator.AbstractCob2JavaGeneratorMain;

/**
 * Exposes the {@link Cob2JaxbGenerator} utility as a command line tool.
 *
 */
public class Cob2JaxbGeneratorMain extends AbstractCob2JavaGeneratorMain {

    private static Logger log = LoggerFactory
            .getLogger(Cob2JaxbGeneratorMain.class);

    /**
     * @param args generator options. Provides help if no arguments passed.
     */
    public static void main(final String[] args) {
        Cob2JaxbGeneratorMain main = new Cob2JaxbGeneratorMain();
        main.execute(args);
    }

    public void generate(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output, String packageNamePrefix,
            final String xsltFileName) {
        log.info("Processing COBOL file " + cobolFile);
        Cob2JaxbGenerator gen = new Cob2JaxbGenerator(
                configProps);
        String baseName = FilenameUtils.getBaseName(
                cobolFile.getAbsolutePath()).toLowerCase();
        String packageName = packageNamePrefix == null ? baseName
                : (packageNamePrefix + "." + baseName);

        gen.generate(cobolFile,
                cobolFileEncoding, output, packageName, xsltFileName);
    }

}
