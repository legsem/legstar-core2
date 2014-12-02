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
public class Cob2ConverterGenerator {

    private final Cob2Xsd cob2xsd;

    private final Xsd2ConverterGenerator xsd2Converter;

    public Cob2ConverterGenerator(Properties configProps) {
        cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
        xsd2Converter = new Xsd2ConverterGenerator();
    }

    /**
     * Given a COBOL copybook, produce a set of java classes (source code) used
     * to convert mainframe data (matching the copybook) at runtime.
     * 
     * @param cobolSource the COBOL copybook source
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(String cobolSource,
            String packageName) {

        return generate(new StringReader(cobolSource), packageName);

    }

    /**
     * Given a COBOL copybook, produce a set of java classes (source code) used
     * to convert mainframe data (matching the copybook) at runtime.
     * 
     * @param cobolFile the COBOL copybook file
     * @param cobolFileEncoding the COBOL copybook file character encoding
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(File cobolFile,
            String cobolFileEncoding, String packageName) {
        try {
            Reader reader = cobolFileEncoding == null ? new InputStreamReader(
                    new FileInputStream(cobolFile)) : new InputStreamReader(
                    new FileInputStream(cobolFile), cobolFileEncoding);
            return generate(reader, packageName);
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
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     */
    public Map < String, String > generate(Reader cobolReader,
            String packageName) {

        String xmlSchemaSource = cob2xsd.translate(cobolReader);
        return xsd2Converter.generate(xmlSchemaSource, packageName);

    }

}
