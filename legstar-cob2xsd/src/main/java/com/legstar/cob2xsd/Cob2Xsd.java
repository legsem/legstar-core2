/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.TreeNodeStream;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaForm;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cob2xsd.antlr.CleanerException;
import com.legstar.cob2xsd.antlr.RecognizerException;
import com.legstar.cob2xsd.antlr.CobolFixedFormatSourceCleaner;
import com.legstar.cob2xsd.antlr.CobolFreeFormatSourceCleaner;
import com.legstar.cobol.CobolStructureEmitter;
import com.legstar.cob2xsd.antlr.CobolStructureEmitterImpl;
import com.legstar.cobol.CobolStructureLexer;
import com.legstar.cob2xsd.antlr.CobolStructureLexerImpl;
import com.legstar.cobol.CobolStructureParser;
import com.legstar.cobol.CobolStructureParser.cobdata_return;
import com.legstar.cob2xsd.antlr.CobolStructureParserImpl;
import com.legstar.cob2xsd.antlr.RecognizerErrorHandler;
import com.legstar.cobol.model.CobolDataItem;

/**
 * Implements the COBOL Structure to XML Schema translator. This is the API made
 * available to invoke the COBOL to XML Schema translator from your own java
 * code.
 * <p/>
 * There are 6 steps involved:
 * <ul>
 * <li>Cleaning the source from non COBOL Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful COBOL statements</li>
 * <li>Emitting a COBOL model (a set of java classes) from the Abstract Syntax
 * Tree</li>
 * <li>Emitting XML Schema from the COBOL model</li>
 * <li>Writing the XML Schema, optionally applying a customization XSLT</li>
 * </ul>
 * <p/>
 * All options are bundled in {@link Cob2XsdModel} instance that is received at
 * construction time.
 * <p/>
 * To invoke the translator, you normally call the translate method.
 * <p/>
 * Any error encountered and recovered from is available in
 * {@link #getErrorHistory()}.
 * 
 */
public class Cob2Xsd {

    /** Execution parameters for the COBOL to XSD utility. */
    private final Cob2XsdConfig _config;

    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(Cob2Xsd.class);

    /** Handles error messages. */
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * @param config configuration parameters for the COBOL to XML Schema utility
     */
    public Cob2Xsd(final Cob2XsdConfig config) {
        _config = config;
    }

    /**
     * Execute the translation from COBOL to XML Schema.
     * 
     * @param cobolReader reads the raw COBOL source
     * @param targetNamespace the generated XML schema target namespace (null
     *            for no namespace)
     * @return the XML Schema
     * @throws XsdGenerationException if XML schema generation process fails
     */
    public String translate(final Reader cobolReader,
            final String targetNamespace) throws XsdGenerationException {
        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Translating with options: {}", getConfig()
                        .toString());
                _log.debug("Target namespace: {}", targetNamespace);
            }
            return xsdToString(emitXsd(toModel(cobolReader), targetNamespace));
        } catch (RecognizerException e) {
            throw new XsdGenerationException(e);
        }
    }

    /**
     * Parses a COBOL source into an in-memory model.
     * 
     * @param cobolReader reads the raw COBOL source
     * @return a list of root COBOL data items
     * @throws RecognizerException if COBOL recognition fails
     */
    public List < CobolDataItem > toModel(final Reader cobolReader)
            throws RecognizerException {
        return emitModel(parse(lex(clean(cobolReader))));
    }

    /**
     * Remove any non COBOL Structure characters from the source.
     * 
     * @param cobolReader reads the raw COBOL source
     * @return a cleaned up source
     * @throws CleanerException if source cannot be read
     */
    public String clean(final Reader cobolReader) throws CleanerException {
        if (_log.isDebugEnabled()) {
            _log.debug("1. Cleaning COBOL source code");
        }
        switch (getConfig().getCodeFormat()) {
        case FIXED_FORMAT:
            CobolFixedFormatSourceCleaner fixedCleaner = new CobolFixedFormatSourceCleaner(
                    getErrorHandler(), getConfig().getStartColumn(), getConfig()
                            .getEndColumn());
            return fixedCleaner.clean(cobolReader);
        case FREE_FORMAT:
            CobolFreeFormatSourceCleaner freeCleaner = new CobolFreeFormatSourceCleaner(
                    getErrorHandler());
            return freeCleaner.clean(cobolReader);
        default:
            throw new CleanerException("Unkown COBOL source format "
                    + getConfig().getCodeFormat());
        }
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * 
     * @param cleanedCobolSource the source code (clean outside columns 7 to 72)
     * @return an antlr token stream
     * @throws RecognizerException if lexer failed to tokenize COBOL source
     */
    public CommonTokenStream lex(final String cleanedCobolSource)
            throws RecognizerException {
        if (_log.isDebugEnabled()) {
            _log.debug("2. Lexing COBOL source code: {}", cleanedCobolSource);
        }
        try {
            CobolStructureLexer lex = new CobolStructureLexerImpl(
                    new ANTLRReaderStream(new StringReader(cleanedCobolSource)),
                    getErrorHandler());
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() != 0 || tokens == null) {
                throw (new RecognizerException("Lexing failed. "
                        + lex.getNumberOfSyntaxErrors() + " syntax errors."
                        + " Last error was "
                        + getErrorHistory().get(getErrorHistory().size() - 1)));
            }
            return tokens;
        } catch (IOException e) {
            throw (new RecognizerException(e));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream.
     * 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     * @throws RecognizerException if source contains unsupported statements
     */
    public CommonTree parse(final CommonTokenStream tokens)
            throws RecognizerException {
        if (_log.isDebugEnabled()) {
            _log.debug("3. Parsing tokens: {}", tokens.toString());
        }
        try {
            CobolStructureParser parser = new CobolStructureParserImpl(tokens,
                    getErrorHandler());
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                throw (new RecognizerException("Parsing failed. "
                        + parser.getNumberOfSyntaxErrors() + " syntax errors."
                        + " Last error was "
                        + getErrorHistory().get(getErrorHistory().size() - 1)));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            throw (new RecognizerException(e));
        }
    }

    /**
     * Generates a model from an Abstract Syntax Tree.
     * 
     * @param ast the abstract syntax tree produced by parser
     * @return a list of root COBOL data items
     * @throws RecognizerException if tree cannot be walked
     */
    public List < CobolDataItem > emitModel(final CommonTree ast)
            throws RecognizerException {
        List < CobolDataItem > cobolDataItems = new ArrayList < CobolDataItem >();
        if (_log.isDebugEnabled()) {
            _log.debug("4. Emitting Model from AST: {}",
                    ((ast == null) ? "null" : ast.toStringTree()));
        }
        if (ast == null) {
            return cobolDataItems;
        }
        try {
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            CobolStructureEmitter emitter = new CobolStructureEmitterImpl(
                    nodes, getErrorHandler());
            emitter.cobdata(cobolDataItems);
            return cobolDataItems;
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * Generate an XML Schema using a model of COBOL data items. The model is a
     * list of root level items. From these, we only process group items
     * (structures) with children.
     * 
     * @param cobolDataItems a list of COBOL data items
     * @param targetNamespace the target namespace to use (null for no namespace)
     * @return the XML schema
     */
    public XmlSchema emitXsd(final List < CobolDataItem > cobolDataItems, final String targetNamespace) {
        if (_log.isDebugEnabled()) {
            _log.debug("5. Emitting XML Schema from model: {}",
                    cobolDataItems.toString());
        }
        XmlSchema xsd = createXmlSchema(getConfig().getXsdEncoding(), targetNamespace);
        List < String > nonUniqueCobolNames = getNonUniqueCobolNames(cobolDataItems);
        XsdEmitter emitter = new XsdEmitter(xsd, getConfig());
        for (CobolDataItem cobolDataItem : cobolDataItems) {
            if (getConfig().ignoreOrphanPrimitiveElements()
                    && cobolDataItem.getChildren().size() == 0) {
                continue;
            }
            XsdDataItem xsdDataItem = new XsdDataItem(cobolDataItem,
                    getConfig(), null, 0, nonUniqueCobolNames, _errorHandler);
            // Create and add a root element
            xsd.getItems().add(emitter.createXmlSchemaElement(xsdDataItem));
        }
        return xsd;
    }

    /**
     * Serialize the XML Schema to a string.
     * <p/>
     * If we are provided with an XSLT customization file then we transform the
     * XMLSchema.
     * 
     * @param xsd the XML Schema before customization
     * @return a string serialization of the customized XML Schema
     * @throws XsdGenerationException if customization fails
     */
    public String xsdToString(final XmlSchema xsd)
            throws XsdGenerationException {

        if (_log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            xsd.write(writer);
            _log.debug("6. Writing XML Schema: {}", writer.toString());
        }

        String errorMessage = "Customizing XML Schema failed.";
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            try {
                tFactory.setAttribute("indent-number", "4");
            } catch (IllegalArgumentException e) {
                _log.debug("Unable to set indent-number on transfomer factory",
                        e);
            }
            StringWriter writer = new StringWriter();
            Source source = new DOMSource(xsd.getAllSchemas()[0]);
            Result result = new StreamResult(writer);
            Transformer transformer;
            String xsltFileName = getConfig().getCustomXsltFileName();
            if (xsltFileName == null || xsltFileName.trim().length() == 0) {
                transformer = tFactory.newTransformer();
            } else {
                Source xsltSource = new StreamSource(new File(xsltFileName));
                transformer = tFactory.newTransformer(xsltSource);
            }
            transformer.setOutputProperty(OutputKeys.ENCODING, getConfig()
                    .getXsdEncoding());
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer
                    .setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
            transformer.setOutputProperty(OutputKeys.STANDALONE, "no");

            transformer.transform(source, result);
            writer.flush();

            return writer.toString();
        } catch (TransformerConfigurationException e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        } catch (TransformerException e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        }
    }

    /**
     * Create a list of COBOL names which are not unique. This is useful when we
     * build complex type names from the COBOL names. Complex type names need to
     * be unique within a target namespace.
     * 
     * @param cobolDataItems a list of root data items
     * @return a list of COBOL names which are not unique
     */
    public static List < String > getNonUniqueCobolNames(
            final List < CobolDataItem > cobolDataItems) {
        List < String > cobolNames = new ArrayList < String >();
        List < String > nonUniqueCobolNames = new ArrayList < String >();
        for (CobolDataItem cobolDataItem : cobolDataItems) {
            getNonUniqueCobolNames(cobolDataItem, cobolNames,
                    nonUniqueCobolNames);
        }
        return nonUniqueCobolNames;
    }

    /**
     * If data item COBOL name is already used, we add it to the non unique
     * list. This recurse to the item children.
     * <p/>
     * We don't add COBOL FILLERs as they are always considered non unique.
     * 
     * @param cobolDataItem a COBOL data item
     * @param cobolNames the list of all COBOL names used so far
     * @param nonUniqueCobolNames the list of non unique COBOL names
     */
    protected static void getNonUniqueCobolNames(
            final CobolDataItem cobolDataItem,
            final List < String > cobolNames,
            final List < String > nonUniqueCobolNames) {

        String cobolName = cobolDataItem.getCobolName();
        if (!cobolName.equalsIgnoreCase("FILLER")) {
            if (cobolNames.contains(cobolName)) {
                if (!nonUniqueCobolNames.contains(cobolName)) {
                    nonUniqueCobolNames.add(cobolName);
                }
            } else {
                cobolNames.add(cobolName);
            }
        }
        for (CobolDataItem child : cobolDataItem.getChildren()) {
            getNonUniqueCobolNames(child, cobolNames, nonUniqueCobolNames);
        }
    }

    /**
     * Create an empty XML Schema.
     * <p/>
     * If no targetNamespace, make sure there is no default namespace otherwise
     * our complex types would be considered part of that default namespace
     * (usually XML Schema namespace).
     * 
     * @param encoding the character set used to encode this XML Schema
     * @param targetNamespace the target namespace to use (null for no namespace)
     * @return a new empty XML schema using the model
     */
    protected XmlSchema createXmlSchema(final String encoding, final String targetNamespace) {
        XmlSchema xsd = new XmlSchema(targetNamespace,
                new XmlSchemaCollection());
        if (targetNamespace != null) {
            xsd.setElementFormDefault(XmlSchemaForm.QUALIFIED);
        }
        xsd.setAttributeFormDefault(null);
        xsd.setInputEncoding(encoding);
        if (targetNamespace == null) {
            NamespaceMap prefixmap = new NamespaceMap();
            NamespacePrefixList npl = xsd.getNamespaceContext();
            if (npl == null) {
                prefixmap.add("xsd", XMLConstants.W3C_XML_SCHEMA_NS_URI);
            } else {
                for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
                    String prefix = npl.getDeclaredPrefixes()[i];
                    String namespace = npl.getNamespaceURI(prefix);
                    if (namespace.equals(XMLConstants.W3C_XML_SCHEMA_NS_URI)) {
                        if (prefix.equals("")) {
                            prefix = "xsd";
                        }
                    }
                    prefixmap.add(prefix, namespace);
                }
            }
            xsd.setNamespaceContext(prefixmap);
        }
        return xsd;
    }

    /**
     * @return the error messages handler
     */
    private RecognizerErrorHandler getErrorHandler() {
        return _errorHandler;
    }

    /**
     * The execution parameters for the COBOL to XML Schema utility.
     * 
     * @return the execution parameters for the COBOL to XML Schema utility
     */
    public Cob2XsdConfig getConfig() {
        return _config;
    }

    /**
     * list of errors encountered while translating.
     * <p/>
     * Most of these errors are warnings which were recovered from but still
     * denote something that user should know about.
     * 
     * @return the list of errors encountered while translating
     */
    public List < String > getErrorHistory() {
        return getErrorHandler().getErrorMessages();
    }
}
