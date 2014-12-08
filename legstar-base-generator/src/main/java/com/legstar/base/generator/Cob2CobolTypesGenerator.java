package com.legstar.base.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.Properties;

import com.legstar.base.utils.NamespaceUtils;
import com.legstar.cob2xsd.Cob2Xsd;
import com.legstar.cob2xsd.Cob2XsdConfig;

/**
 * Given a COBOL copybook, this will produce a set of java classes used at
 * runtime to convert mainframe data to java.
 * <p/>
 * This is a 2 steps process:
 * <ul>
 * <li>Translate the copybook to a COBOL annotated XML schema</li>
 * <li>Generate conversion classes using that XML schema</li>
 * </ul>
 *
 */
public class Cob2CobolTypesGenerator {

    private final Cob2Xsd cob2xsd;

    private final Xsd2CobolTypesGenerator xsd2Converter;

    public Cob2CobolTypesGenerator(Properties configProps) {
        cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
        xsd2Converter = new Xsd2CobolTypesGenerator();
    }

    /**
     * Given a COBOL copybook, produce a set of java classes (source code) used
     * to convert mainframe data (matching the copybook) at runtime.
     * 
     * @param cobolSource the COBOL copybook source
     * @param targetPackageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(String cobolSource,
            String targetPackageName) {

        return generate(new StringReader(cobolSource), targetPackageName);

    }

    /**
     * Given a COBOL copybook, produce a set of java classes (source code) used
     * to convert mainframe data (matching the copybook) at runtime.
     * 
     * @param cobolFile the COBOL copybook file
     * @param cobolFileEncoding the COBOL copybook file character encoding
     * @param targetPackageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(File cobolFile,
            String cobolFileEncoding, String targetPackageName) {
        try {
            Reader reader = cobolFileEncoding == null ? new InputStreamReader(
                    new FileInputStream(cobolFile)) : new InputStreamReader(
                    new FileInputStream(cobolFile), cobolFileEncoding);
            return generate(reader, targetPackageName);
        } catch (UnsupportedEncodingException e) {
            throw new Xsd2ConverterException(e);
        } catch (FileNotFoundException e) {
            throw new Xsd2ConverterException(e);
        }
    }

    /**
     * Given a COBOL copybook, produce a set of java classes (source code) used
     * to convert mainframe data (matching the copybook) at runtime.
     * 
     * @param cobolReader reads the COBOL copybook source
     * @param targetPackageName the java package the generated classes should
     *            reside in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(Reader cobolReader,
            String targetPackageName) {

        // The XML schema is an intermediary result which we do not keep. The
        // target namespace is not useful in this case
        String xmlSchemaSource = cob2xsd.translate(cobolReader,
                NamespaceUtils.toNamespace(targetPackageName));
        return xsd2Converter.generate(xmlSchemaSource, targetPackageName);

    }

}
