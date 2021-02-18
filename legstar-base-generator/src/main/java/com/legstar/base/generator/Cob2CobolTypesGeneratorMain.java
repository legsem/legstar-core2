package com.legstar.base.generator;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Exposes the {@link Cob2CobolTypesGenerator} utility as a command line tool.
 *
 */
public class Cob2CobolTypesGeneratorMain extends AbstractCob2JavaGeneratorMain {

    private static Logger log = LoggerFactory
            .getLogger(Cob2CobolTypesGeneratorMain.class);

    /**
     * @param args generator options. Provides help if no arguments passed.
     */
    public static void main(final String[] args) {
        Cob2CobolTypesGeneratorMain main = new Cob2CobolTypesGeneratorMain();
        main.execute(args);
    }

    public void generate(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output, String packageNamePrefix,
            final String xsltFileName) {
        try {
            log.info("Processing COBOL file " + cobolFile);
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
                log.info("Writing java class " + className + " with package "
                        + packageName + " in " + output);
                FileUtils.writeStringToFile(new File(output, subFolder
                        + className), entry.getValue(), StandardCharsets.UTF_8);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
