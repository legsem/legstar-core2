package com.legstar.cob2xsd;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
     * <p/>
     * When requested the file base name is appended to the target namespace.
     * 
     * @param cobolFile COBOL source file
     * @param target target file or folder
     * @return the XML Schema
     * @throws RecognizerException if parser fails
     * @throws XsdGenerationException if COBOL model interpretation fails
     */
    public File translate(final File cobolFile, final File target) throws RecognizerException,
            XsdGenerationException {

        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Translating COBOL file: " + cobolFile);
            }
            checkCobolSourceFile(cobolFile);
            checkTarget(target);

            String baseName = FilenameUtils.getBaseName(
                    cobolFile.getAbsolutePath()).toLowerCase();

            String xsdString = translate(FileUtils.readFileToString(cobolFile,
                    getConfig().getCobolSourceFileEncoding()), baseName);
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
            String extension = FilenameUtils.getExtension(target.getName());
            if (extension.length() == 0) {
                throw new IOException("Target folder " + target + " not found");
            }
        }
    }

}
