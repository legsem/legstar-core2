package com.legstar.jaxb.generator;

import java.io.File;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXParseException;

import com.legstar.base.generator.Xsd2ConverterGenerator;
import com.sun.codemodel.JCodeModel;
import com.sun.tools.xjc.AbortException;
import com.sun.tools.xjc.ErrorReceiver;
import com.sun.tools.xjc.ModelLoader;
import com.sun.tools.xjc.Options;
import com.sun.tools.xjc.model.Model;

public class AppTest {

    private static final String SRC_TEST_XSDS = "src/test/xsds/";

    private static final String TARGET_JAVA_DIR = "target/generated-sources/java";

    private File javaGenDir;

    @Before
    public void setUp() throws Exception {
        javaGenDir = new File(TARGET_JAVA_DIR);
        FileUtils.deleteQuietly(javaGenDir);
        FileUtils.forceMkdir(javaGenDir);

    }

    /** Logging. */
    private static Logger log = LoggerFactory.getLogger(AppTest.class);

    @Test
    public void testGen() throws Exception {
        xjc("flat01");
        xsd2Convert("flat01");
    }

    private void xsd2Convert(String name) throws Exception {
        File xsdFile = new File(SRC_TEST_XSDS + name + ".xsd");
        Xsd2ConverterGenerator gen = new Xsd2ConverterGenerator();
        Map < String, String > code = gen.generate(xsdFile, "UTF-8",
                "legstar.jaxb.test.converter");
        String className = StringUtils.capitalize(name) + "RecordFactory";
        FileUtils.writeStringToFile(new File(javaGenDir,
                "legstar/jaxb/test/converter/" + className + ".java"), code
                .get(className));
    }

    private void xjc(String name) throws Exception {
        // TODO: I don't know if I should send output to stdout
        ErrorReceiver errorReceiver = new ErrorReceiverImpl();

        Options options = new Options();
        options.parseArguments(new String[] { "-nv", SRC_TEST_XSDS + name + ".xsd" });

        Model model = ModelLoader
                .load(options, new JCodeModel(), errorReceiver);

        if (model == null) {
            throw new XJCException(
                    "unable to parse the schema. Error messages should have been provided");
        }

        if (model.generateCode(options, errorReceiver) == null) {
            throw new XJCException("failed to compile a schema");
        }

        log.info("Writing output to {} ", javaGenDir);

        model.codeModel.build(javaGenDir);
    }

    private class ErrorReceiverImpl extends ErrorReceiver {

        public void error(SAXParseException exception) throws AbortException {
            log.error("XML syntax error", exception);

        }

        public void fatalError(SAXParseException exception)
                throws AbortException {
            log.error("XML syntax fatal error", exception);

        }

        public void warning(SAXParseException exception) throws AbortException {
            log.warn("XML syntax warning", exception);
        }

        public void info(SAXParseException exception) {
            log.info("XML syntax info", exception);
        }

    }
    
    private class XJCException extends RuntimeException {

        private static final long serialVersionUID = 5632557954843880411L;
        
        public XJCException() {
            super();
        }

        public XJCException(String message, Throwable cause) {
            super(message, cause);
        }

        public XJCException(String message) {
            super(message);
        }

        public XJCException(Throwable cause) {
            super(cause);
        }

    }

}
