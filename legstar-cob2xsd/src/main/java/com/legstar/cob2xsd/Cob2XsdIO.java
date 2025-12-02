package com.legstar.cob2xsd;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.base.utils.FileUtils;
import com.legstar.base.utils.FilenameUtils;
import com.legstar.cob2xsd.antlr.RecognizerException;

/**
 * Common code for file translation (as opposed to string translation).
 * 
 */
public class Cob2XsdIO extends Cob2Xsd {

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(Cob2XsdIO.class);

    public Cob2XsdIO(final Cob2XsdConfig config) {
        super(config);
    }

    /**
     * Translates a single COBOL source file.
     * <p>
     * When requested the file base name is appended to the target namespace.
     * 
     * @param cobolFile COBOL source file
     * @param cobolFileEncoding COBOL source file encoding (null to use platform
     *            default)
     * @param target target file or folder
     * @param targetNamespacePrefix used as a prefix prepended to the cobol file
     *            base name to form a unique target namespace for the generated
     *            XML schema
     * @param xsltFileName an optional xslt to apply on the XML Schema
     * @return the XML Schema
     * @throws RecognizerException if parser fails
     * @throws XsdGenerationException if COBOL model interpretation fails
     */
    public File translate(final File cobolFile, final String cobolFileEncoding,
            final File target, final String targetNamespacePrefix,
            final String xsltFileName) throws RecognizerException,
            XsdGenerationException {

        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Translating COBOL file: " + cobolFile);
            }
            checkCobolSourceFile(cobolFile);
            checkTarget(target);

            String baseName = FilenameUtils.getBaseName(
                    cobolFile).toLowerCase();

            Reader cobolReader = cobolFileEncoding == null ? new InputStreamReader(
                    new FileInputStream(cobolFile)) : new InputStreamReader(
                    new FileInputStream(cobolFile), cobolFileEncoding);

            String xsdString = translate(cobolReader,
                    getUniqueTargetNamespace(targetNamespacePrefix, baseName),
                    xsltFileName);
            File xsdFile = null;
            if (target.isDirectory()) {
                String xsdFileName = cobolFile.getName() + ".xsd";
                xsdFile = new File(target, xsdFileName);
            } else {
                xsdFile = target;
            }
            FileUtils.writeStringToFile(xsdFile, xsdString, getConfig()
                    .getXsdEncoding());
            if (_log.isDebugEnabled()) {
                _log.debug("Created XML schema file: " + xsdFile);
            }
            return xsdFile;
        } catch (IOException e) {
            throw (new XsdGenerationException(e));
        }
    }

    /**
     * make sure the COBOL file is valid.
     * 
     * @param cobolSourceFile the COBOL source file
     * @throws IOException if file cannot be located
     */
    protected void checkCobolSourceFile(final File cobolSourceFile)
            throws IOException {
        if (cobolSourceFile == null) {
            throw new IOException("You must provide a COBOL source file");
        }
        if (!cobolSourceFile.exists()) {
            throw new IOException("COBOL source  file " + cobolSourceFile
                    + " not found");
        }
    }

    /**
     * make sure the target, folder or file, is valid. We consider that target
     * files will have extensions. Its ok for a target file not to exist but
     * target folders must exist.
     * 
     * @param target the target folder or file
     * @throws IOException if file cannot be located
     */
    protected void checkTarget(final File target) throws IOException {
        if (target == null) {
            throw new IOException("You must provide a target directory or file");
        }
        if (!target.exists()) {
            String extension = FilenameUtils.getExtension(target);
            if (extension.length() == 0) {
                throw new IOException("Target folder " + target + " not found");
            }
        }
    }

    /**
     * TargetNamespace, if it is not null, is completed with the baseName.
     * 
     * @param targetNamespacePrefix the target namespace prefix
     * @param baseName A name, derived from the COBOL file name, that can be
     *            used to identify generated artifacts
     * @return the targetNamespace for the xml schema
     * @throws IOException if unable to create a namespace
     */
    protected String getUniqueTargetNamespace(String targetNamespacePrefix,
            final String baseName) throws IOException {

        if (targetNamespacePrefix == null
                || targetNamespacePrefix.length() == 0) {
            return null;
        }

        if (baseName == null || baseName.length() == 0) {
            throw new IOException("No target basename was provided");
        }

        if (targetNamespacePrefix.charAt(targetNamespacePrefix.length() - 1) == '/') {
            return targetNamespacePrefix + baseName;
        } else {
            return targetNamespacePrefix + '/' + baseName;
        }
    }

}
