package com.legstar.base.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.IOUtils;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;

/**
 * Produces a set of java classes for runtime conversion of mainframe data.
 * <p/>
 * The generated java classes reflect the structure of an original COBOL
 * copybook after it was translated to a COBOL annotated XML Schema ( see
 * legstar-cob2xsd).
 *
 */
public class Xsd2CobolTypesGenerator {

    public static final String JAVA_CLASS_NAME_PREFIX = "Cobol";

    public static final String JAVA_CLASS_TEMPLATE_NAME = "java.class.hbs";

    /** Handlebars template for a java class. */
    private final Template hbtJavaClass;

    private final Xsd2CobolTypesModelBuilder modelBuilder;

    public Xsd2CobolTypesGenerator() {
        try {
            String text = IOUtils.toString(getClass().getResourceAsStream(
                    JAVA_CLASS_TEMPLATE_NAME), StandardCharsets.UTF_8);
            Handlebars handlebars = new Handlebars();
            hbtJavaClass = handlebars.compileInline(text);
            modelBuilder = new Xsd2CobolTypesModelBuilder();
        } catch (IOException e) {
            throw new Xsd2ConverterException(e);
        }
    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data at runtime.
     * 
     * @param xmlSchemaFile the COBOL-annotated XML schema file (see legstar-cob2xsd)
     * @param encoding the character encoding for xmlSchemaFile
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(File xmlSchemaFile, String encoding,
            String packageName) throws Xsd2ConverterException {
        try {
            Reader reader = new InputStreamReader(new FileInputStream(xmlSchemaFile),
                    encoding);
            return generate(reader, packageName);
        } catch (UnsupportedEncodingException e) {
            throw new Xsd2ConverterException(e);
        } catch (FileNotFoundException e) {
            throw new Xsd2ConverterException(e);
        }
    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data at runtime.
     * 
     * @param xmlSchemaSource the COBOL-annotated XML schema source (see legstar-cob2xsd)
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(String xmlSchemaSource,
            String packageName) throws Xsd2ConverterException {
        return generate(new StringReader(xmlSchemaSource), packageName);
    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data at runtime.
     * 
     * @param reader reads the COBOL-annotated XML schema source (see legstar-cob2xsd)
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(Reader reader,
            String packageName) throws Xsd2ConverterException {

        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema xsd = schemaCol.read(new StreamSource(reader));
        return generate(xsd, packageName);
        
    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data at runtime.
     * 
     * @param xmlSchema the COBOL-annotated XML schema (see legstar-cob2xsd)
     * @param targetPackageName the java package the generated classes should
     *            reside in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(XmlSchema xmlSchema,
            String targetPackageName) throws Xsd2ConverterException {
        try {
            Map < String, String > code = new HashMap < String, String >();
            for (Entry < String, Xsd2CobolTypesModelBuilder.RootCompositeType > entry : modelBuilder
                    .build(xmlSchema).entrySet()) {
                String className = JAVA_CLASS_NAME_PREFIX + entry.getKey();
                Map < String, Object > model = new HashMap < String, Object >();
                if (targetPackageName != null && targetPackageName.length() > 0) {
                    model.put("target_package_name", targetPackageName);
                }
                model.put("class_name", className);
                model.put("root_type_name", entry.getKey());
                model.put("root_cobol_name", entry.getValue().cobolName);
                model.put("complex_types", entry.getValue().complexTypes);
                model.put("choice_types", entry.getValue().choiceTypes);
                code.put(className, hbtJavaClass.apply(model));
            }
            return code;
        } catch (IOException e) {
            throw new Xsd2ConverterException(e);
        }
    }

}
