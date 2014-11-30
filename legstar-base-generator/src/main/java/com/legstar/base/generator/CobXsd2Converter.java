package com.legstar.base.generator;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.io.IOUtils;
import org.apache.ws.commons.schema.XmlSchema;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;

public class CobXsd2Converter {

    public static final String JAVA_CLASS_TEMPLATE_NAME = "java.class.hbs";

    /** Handlebars template for a java class. */
    private final Template hbtJavaClass;
    
    private final CobXsd2ConverterModelBuilder modelBuilder;
    
    public CobXsd2Converter() {
        try {
            String text = IOUtils.toString(getClass().getResourceAsStream(
                    JAVA_CLASS_TEMPLATE_NAME));
            Handlebars handlebars = new Handlebars();
            hbtJavaClass = handlebars.compileInline(text);
            modelBuilder = new CobXsd2ConverterModelBuilder();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public Map <String, String> generate(XmlSchema xmlSchema, String packageName) throws IOException {
        Map <String, String> code = new HashMap <String, String>() ;
        for (Entry <String, CobXsd2ConverterModelBuilder.CompositeTypes> entry : modelBuilder.build(xmlSchema).entrySet()) {
            String className = entry.getKey() + "Factory";
            Map < String, Object > model = new HashMap < String, Object > ();
            model.put("package_name", packageName);
            model.put("class_name", className);
            model.put("root_type_name", entry.getKey());
            model.put("complex_types", entry.getValue().complexTypes);
            model.put("choice_types", entry.getValue().choiceTypes);
            code.put(className, hbtJavaClass.apply(model));
        }
        return code;
    }

}
