package com.legstar.jaxb.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXParseException;

import com.legstar.base.generator.Xsd2CobolTypesGenerator;
import com.legstar.base.utils.NamespaceUtils;
import com.legstar.cob2xsd.Cob2Xsd;
import com.legstar.cob2xsd.Cob2XsdConfig;
import com.sun.codemodel.JCodeModel;
import com.sun.tools.xjc.AbortException;
import com.sun.tools.xjc.BadCommandLineException;
import com.sun.tools.xjc.ErrorReceiver;
import com.sun.tools.xjc.ModelLoader;
import com.sun.tools.xjc.Options;
import com.sun.tools.xjc.model.Model;

public class Cob2JaxbGenerator {

    private static Logger log = LoggerFactory
            .getLogger(Cob2JaxbGenerator.class);

    private final Cob2Xsd cob2xsd;

    private final Xsd2CobolTypesGenerator xsd2CobolTypes;

    private final Xsd2JaxbGenerator xsd2JaxbWrappers;

    public Cob2JaxbGenerator(Properties configProps) {
        cob2xsd = new Cob2Xsd(new Cob2XsdConfig(configProps));
        xsd2CobolTypes = new Xsd2CobolTypesGenerator();
        xsd2JaxbWrappers = new Xsd2JaxbGenerator();
    }

    /**
     * Given a COBOL copybook in a file, produce a set of java classes (source
     * code) used to convert mainframe data (matching the copybook) to JAXB
     * instances.
     * 
     * @param cobolFile the COBOL copybook file
     * @param cobolFileEncoding the COBOL copybook file character encoding
     * @param targetFolder the target folder
     * @param targetPackageName the java package the generated classes should
     *            reside in
     * @param xsltFileName an optional xslt to apply on the XML Schema
     */
    public void generate(File cobolFile, String cobolFileEncoding, File targetFolder,
            String targetPackageName,
            final String xsltFileName) {
        try {
            Reader reader = cobolFileEncoding == null ? new InputStreamReader(
                    new FileInputStream(cobolFile)) : new InputStreamReader(
                    new FileInputStream(cobolFile), cobolFileEncoding);
            generate(reader, targetFolder, targetPackageName, xsltFileName);
        } catch (UnsupportedEncodingException e) {
            throw new Cob2JaxbGeneratorException(e);
        } catch (FileNotFoundException e) {
            throw new Cob2JaxbGeneratorException(e);
        }
    }

    /**
     * Given a COBOL copybook in a reader, produce a set of java classes (source
     * code) used to convert mainframe data (matching the copybook) to JAXB
     * instances.
     * 
     * @param cobolReader the COBOL copybook reader
     * @param targetFolder the target folder
     * @param targetPackageName the java package the generated classes should
     *            reside in
     * @param xsltFileName an optional xslt to apply on the XML Schema
     */
    public void generate(Reader cobolReader,
            File targetFolder, String targetPackageName,
            final String xsltFileName) {

        log.info("Step 1: translate COBOL copybook to annotated XML schema");
        String xmlSchemaSource = cob2xsd.translate(cobolReader,
                NamespaceUtils.toNamespace(targetPackageName), xsltFileName);

        log.info("Step 2: invoke JAXB-XJC utility");
        xjc(xmlSchemaSource, targetFolder, targetPackageName);

        Map < String, String > code = new HashMap < String, String >();
        code.putAll(xsd2CobolTypes.generate(xmlSchemaSource, targetPackageName));
        code.putAll(xsd2JaxbWrappers.generate(xmlSchemaSource,
                targetPackageName));
        outputCode(code, targetFolder, targetPackageName);

    }

    private void outputCode(Map < String, String > code, File targetFolder,
            String targetPackageName) {
        try {
            String subFolder = targetPackageName == null ? ""
                    : (targetPackageName.replace(".", "/") + "/");
            for (Entry < String, String > entry : code.entrySet()) {
                String className = entry.getKey() + ".java";
                log.info("Writing java class " + className + " with package "
                        + targetPackageName + " in " + targetFolder);
                FileUtils.writeStringToFile(new File(targetFolder, subFolder
                        + className), entry.getValue());
            }
        } catch (IOException e) {
            throw new Cob2JaxbGeneratorException(e);
        }

    }

    /**
     * Invoke the XJC utility which takes input from a file.
     * <p/>
     * First step is to copy the XSD source to a temporary file.
     * <p/>
     * We then build a command line and invoke XJC.
     * 
     * @param xmlSchemaSource the input XML schema
     * @param targetFolder the target folder for generated JAXB classes
     * @param packageName the package name for the JAXB classes
     * @throws Cob2JaxbGeneratorException
     */
    private void xjc(String xmlSchemaSource, File targetFolder, String packageName)
            throws Cob2JaxbGeneratorException {

        try {
            File tempXsdFile = File.createTempFile("legstar", ".xsd");
            FileUtils.writeStringToFile(tempXsdFile, xmlSchemaSource);
            
            ErrorReceiver errorReceiver = new ErrorReceiverImpl();

            Options options = new Options();
            options.parseArguments(new String[] { "-nv", "-npa", "-p",
                    packageName, tempXsdFile.getAbsolutePath() });

            Model model = ModelLoader.load(options, new JCodeModel(),
                    errorReceiver);

            if (model == null) {
                throw new Cob2JaxbGeneratorException(
                        "JAXB-XJC unable to parse the schema. Error messages should have been provided");
            }

            if (model.generateCode(options, errorReceiver) == null) {
                throw new Cob2JaxbGeneratorException(
                        "JAXB-XJC failed to compile a schema");
            }

            model.codeModel.build(targetFolder);
            tempXsdFile.deleteOnExit();
        } catch (IOException e) {
            throw new Cob2JaxbGeneratorException(e);
        } catch (BadCommandLineException e) {
            throw new Cob2JaxbGeneratorException(e);
        }
    }

    private static class ErrorReceiverImpl extends ErrorReceiver {

        public void error(SAXParseException exception) throws AbortException {
            log.error("JAXB-XJC XML syntax error", exception);

        }

        public void fatalError(SAXParseException exception)
                throws AbortException {
            log.error("JAXB-XJC XML syntax fatal error", exception);

        }

        public void warning(SAXParseException exception) throws AbortException {
            log.warn("JAXB-XJC XML syntax warning", exception);
        }

        public void info(SAXParseException exception) {
            log.info("JAXB-XJC XML syntax info", exception);
        }

    }

}
