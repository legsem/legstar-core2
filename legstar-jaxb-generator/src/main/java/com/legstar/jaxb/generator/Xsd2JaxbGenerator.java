package com.legstar.jaxb.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.IOUtils;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.helper.StringHelpers;
import com.legstar.base.generator.Xsd2CobolTypesGenerator;
import com.legstar.base.generator.Xsd2ConverterException;
import com.legstar.base.generator.Xsd2CobolTypesModelBuilder;
import com.legstar.base.utils.NamespaceUtils;

/**
 * Given a COBOL-annotated XML schema, generates converter code for JAXB.
 * <p/>
 * Produces the following artifacts:
 * <ul>
 * <li>Wrappers to get/set JAXB instances properties. They are a faster
 * alternative to using reflection</li>
 * <li>A conversion class ready to be invoked</li>
 * </ul>
 * 
 */
public class Xsd2JaxbGenerator {

    public static final String JAXB_WRAPPER_FACTORY_CLASS_NAME_SUFFIX = "Jaxb";

    public static final String JAXB_WRAPPER_FACTORY_CLASS_TEMPLATE_NAME = "jaxb.wrappers.factory.class.hbs";

    public static final String JAXB_CONVERTER_CLASS_NAME_PREFIX = "Cob2";

    public static final String JAXB_CONVERTER_CLASS_NAME_SUFFIX = "Converter";

    public static final String JAXB_CONVERTER_CLASS_TEMPLATE_NAME = "jaxb.converter.class.hbs";

    /** Handlebars template for a jaxb wrappers factory class. */
    private final Template hbtJaxbWrapperFactoryClass;

    /** Handlebars template for a jaxb converter class. */
    private final Template hbtJaxbConverterClass;

    private final Xsd2CobolTypesModelBuilder modelBuilder;

    public Xsd2JaxbGenerator() {
        Handlebars handlebars = new Handlebars();
        handlebars.registerHelper("capFirst", StringHelpers.capitalize);
        handlebars.registerHelper("each",
                new com.legstar.jaxb.generator.EachHelper());
        hbtJaxbWrapperFactoryClass = loadTemplate(handlebars,
                JAXB_WRAPPER_FACTORY_CLASS_TEMPLATE_NAME);
        hbtJaxbConverterClass = loadTemplate(handlebars,
                JAXB_CONVERTER_CLASS_TEMPLATE_NAME);
        modelBuilder = new Xsd2CobolTypesModelBuilder();
    }

    private Template loadTemplate(Handlebars handlebars, String resourceName) {
        try {
            String text = IOUtils.toString(getClass().getResourceAsStream(
                    resourceName));
            return handlebars.compileInline(text);
        } catch (IOException e) {
            throw new Xsd2ConverterException(e);
        }
    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data at runtime.
     * 
     * @param xmlSchemaFile the COBOL-annotated XML schema file (see
     *            legstar-cob2xsd)
     * @param encoding the character encoding for xmlSchemaFile
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(File xmlSchemaFile, String encoding,
            String packageName) throws Xsd2ConverterException {
        try {
            Reader reader = new InputStreamReader(new FileInputStream(
                    xmlSchemaFile), encoding);
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
     * @param xmlSchemaSource the COBOL-annotated XML schema source (see
     *            legstar-cob2xsd)
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
     * @param reader reads the COBOL-annotated XML schema source (see
     *            legstar-cob2xsd)
     * @param packageName the java package the generated classes should reside
     *            in
     * @return a map of java class names to their source code
     * @throws Xsd2ConverterException if generation fails
     */
    public Map < String, String > generate(Reader reader, String packageName)
            throws Xsd2ConverterException {

        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema xsd = schemaCol.read(new StreamSource(reader));
        return generate(xsd, packageName);

    }

    /**
     * Given a COBOL annotated XML schema, produce a set of java classes (source
     * code) used to convert mainframe data to JAXB instances at runtime.
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
                String jaxbWrappersFactoryclassName = entry.getKey()
                        + JAXB_WRAPPER_FACTORY_CLASS_NAME_SUFFIX;
                Map < String, Object > model = new HashMap < String, Object >();
                if (targetPackageName != null && targetPackageName.length() > 0) {
                    model.put("target_package_name", targetPackageName);
                }
                String jaxbPackageName = NamespaceUtils.toPackageName(xmlSchema
                        .getTargetNamespace());
                if (jaxbPackageName != null && jaxbPackageName.length() > 0) {
                    model.put("jaxb_package_name", jaxbPackageName);
                }
                model.put("class_name", jaxbWrappersFactoryclassName);
                model.put("root_type_name", entry.getKey());
                model.put("complex_types", entry.getValue().complexTypes);
                model.put("choice_types", entry.getValue().choiceTypes);
                code.put(jaxbWrappersFactoryclassName,
                        hbtJaxbWrapperFactoryClass.apply(model));

                String jaxbConverterClassName = JAXB_CONVERTER_CLASS_NAME_PREFIX
                        + entry.getKey() + JAXB_CONVERTER_CLASS_NAME_SUFFIX;
                model.put("class_name", jaxbConverterClassName);
                model.put(
                        "complex_type_class_name",
                        Xsd2CobolTypesGenerator.JAVA_CLASS_NAME_PREFIX
                                + entry.getKey());
                model.put("jaxb_wrappers_factory_class_name",
                        jaxbWrappersFactoryclassName);
                code.put(jaxbConverterClassName,
                        hbtJaxbConverterClass.apply(model));

            }
            return code;
        } catch (IOException e) {
            throw new Xsd2ConverterException(e);
        }
    }

}
