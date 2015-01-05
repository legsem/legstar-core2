package com.legstar.base.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Shared code for command line utilities using COBOL copybooks as input and
 * producing some kind of output java classes from it.
 *
 */
public abstract class AbstractCob2JavaGeneratorMain {

    /** Options that can be setup. */
    private static final String OPTION_INPUT = "input";

    private static final String OPTION_INPUT_ENCODING = "inputEncoding";

    private static final String OPTION_OUTPUT = "output";

    private static final String OPTION_OUTPUT_PACKAGE_PREFIX = "packagePrefix";

    private static final String OPTION_XSLT_FILE_NAME = "xsltFileName";

    private static final String OPTION_CONFIG = "config";

    private static final String OPTION_HELP = "help";

    private static final String OPTION_VERSION = "version";

    private static Logger log = LoggerFactory
            .getLogger(AbstractCob2JavaGeneratorMain.class);

    /** The defaults. */
    private static final String DEFAULT_INPUT_FOLDER_PATH = "cobol";

    private static final String DEFAULT_OUTPUT_FOLDER_PATH = "target/generated-sources";

    public static final Pattern PACKAGE_NAME_PATTERN = Pattern
            .compile("^[a-zA_Z_][\\.\\w]*$");

    /**
     * A COBOL copybook file or a folder containing COBOL copybooks. Defaults to
     * cobol relative folder.
     */
    private File input;

    /**
     * Character encoding used by input files (null means platform default)
     */
    private String inputEncoding;

    /**
     * A folder containing generated artifacts. Defaults to target relative
     * folder.
     */
    private File output;

    /**
     * A package name for output java classes. Optional, default is no package.
     */
    private String packagePrefix;

    /**
     * An optional xslt to apply on the XML Schema before it is used.
     */
    private String xsltFileName;

    /**
     * An optional configuration properties set for the generator.
     */
    private Properties configProps;

    /**
     * Process command line options and run generator.
     * <p/>
     * If no options are passed, prints the help. Help is also printed if the
     * command line options are invalid.
     * 
     * @param args generator options
     */
    public void execute(final String[] args) {
        log.info("Generation started");
        try {
            Options options = createOptions();
            if (collectOptions(options, args)) {
                setDefaults();
                if (input.isDirectory()) {
                    for (File cobolFile : FileUtils
                            .listFiles(input, null, true)) {
                        generate(configProps, cobolFile, inputEncoding, output,
                                packagePrefix, xsltFileName);
                    }
                } else {
                    generate(configProps, input, inputEncoding, output,
                            packagePrefix, xsltFileName);
                }
            }
        } catch (Exception e) {
            log.error("Generation failed", e);
        } finally {
            log.info("Generation ended");
        }
    }

    public abstract void generate(Properties configProps, File cobolFile,
            String cobolFileEncoding, File output, String packageNamePrefix,
            final String xsltFileName);

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", OPTION_VERSION, false,
                "print the version information and exit");
        options.addOption(version);

        Option help = new Option("h", OPTION_HELP, false,
                "print the options available");
        options.addOption(help);

        Option input = new Option("i", OPTION_INPUT, true,
                "file or folder holding the COBOL copybooks to translate."
                        + " Name is relative or absolute");
        options.addOption(input);

        Option inputEncoding = new Option("enc", OPTION_INPUT_ENCODING, true,
                "Character set used for COBOL files encoding");
        options.addOption(inputEncoding);

        Option output = new Option("o", OPTION_OUTPUT, true,
                "folder receiving the generated artifacts");
        options.addOption(output);

        Option outputPackage = new Option("p", OPTION_OUTPUT_PACKAGE_PREFIX,
                true, "Output java package prefix name");
        options.addOption(outputPackage);

        Option config = new Option("c", OPTION_CONFIG, true,
                "optional configuration file");
        options.addOption(config);

        Option xsltFileName = new Option("x", OPTION_XSLT_FILE_NAME,
                true, "an xslt to apply on the XML Schema before it is used");
        options.addOption(xsltFileName);

        return options;
    }

    /**
     * Take arguments received on the command line and setup corresponding
     * options.
     * <p/>
     * No arguments is valid. It means use the defaults.
     * 
     * @param options the expected options
     * @param args the actual arguments received on the command line
     * @return true if arguments were valid
     */
    protected boolean collectOptions(final Options options, final String[] args) {
        try {
            if (args != null && args.length > 0) {
                CommandLineParser parser = new PosixParser();
                CommandLine line = parser.parse(options, args);
                return processLine(line, options);
            }
            return true;
        } catch (ParseException e) {
            log.error("Invalid option", e);
            return false;
        } catch (IllegalArgumentException e) {
            log.error("Invalid option value", e);
            return false;
        }
    }

    /**
     * Process the command line options selected.
     * 
     * @param line the parsed command line
     * @param options available
     * @return false if processing needs to stop, true if its ok to continue
     * @throws Exception if line cannot be processed
     */
    private boolean processLine(final CommandLine line, final Options options) {
        if (line.hasOption(OPTION_VERSION)) {
            log.info(getVersion(true));
            return false;
        }
        if (line.hasOption(OPTION_HELP)) {
            produceHelp(options);
            return false;
        }
        if (line.hasOption(OPTION_INPUT)) {
            setInput(line.getOptionValue(OPTION_INPUT).trim());
        }
        if (line.hasOption(OPTION_INPUT_ENCODING)) {
            setInputEncoding(line.getOptionValue(OPTION_INPUT_ENCODING).trim());
        }
        if (line.hasOption(OPTION_OUTPUT)) {
            setOutput(line.getOptionValue(OPTION_OUTPUT).trim());
        }
        if (line.hasOption(OPTION_OUTPUT_PACKAGE_PREFIX)) {
            setOutputPackagePrefix(line.getOptionValue(
                    OPTION_OUTPUT_PACKAGE_PREFIX).trim());
        }
        if (line.hasOption(OPTION_CONFIG)) {
            setConfigProps(line.getOptionValue(OPTION_CONFIG).trim());
        }
        if (line.hasOption(OPTION_XSLT_FILE_NAME)) {
            setXsltFileName(line.getOptionValue(
                    OPTION_XSLT_FILE_NAME).trim());
        }

        return true;
    }

    /**
     * Retrieve the current version.
     * 
     * @parm verbose when true will also return the build date
     * @return the version number and build date
     */
    private String getVersion(boolean verbose) {
        InputStream stream = null;
        try {
            stream = getClass().getResourceAsStream(
                    "/version.properties");
            Properties props = new Properties();
            props.load(stream);
            if (verbose) {
                return String.format("Version=%s, build date=%s",
                        props.getProperty("version"),
                        props.getProperty("buildDate"));
            } else {
                return props.getProperty("version");
            }
        } catch (IOException e) {
            log.error("Unable to retrieve version", e);
            return "unknown";
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    log.error("Unable to close version resource", e);
                }
            }
        }
    }

    /**
     * @param options options available
     * @throws Exception if help cannot be produced
     */
    private void produceHelp(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("Options available:", options);
    }

    /**
     * Make sure mandatory parameters have default values.
     */
    private void setDefaults() {
        if (input == null) {
            setInput(DEFAULT_INPUT_FOLDER_PATH);
        }
        if (output == null) {
            setOutput(DEFAULT_OUTPUT_FOLDER_PATH);
        }

    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * 
     * @param inputPath a file or folder name (relative or absolute)
     */
    public void setInput(final String inputPath) {
        if (inputPath == null) {
            throw (new IllegalArgumentException(
                    "You must provide a source folder or file"));
        }
        File inputFile = new File(inputPath);
        if (inputFile.exists()) {
            if (inputFile.isDirectory() && inputFile.list().length == 0) {
                throw new IllegalArgumentException("Folder '" + inputPath
                        + "' is empty");
            }
        } else {
            throw new IllegalArgumentException("Input file or folder '"
                    + inputPath + "' not found");
        }
        this.input = inputFile;
    }

    public void setInputEncoding(String inputEncoding) {
        this.inputEncoding = inputEncoding;
    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * 
     * @param outputPath a file or folder name (relative or absolute)
     */
    public void setOutput(final String outputPath) {
        if (outputPath == null) {
            throw (new IllegalArgumentException(
                    "You must provide a target folder or file"));
        }
        this.output = new File(outputPath);
    }

    /**
     * Check that package prefix name provided is valid.
     * <p/>
     * Null is acceptable and interpreted as no prefix.
     * 
     * @param packagePrefix the generated java classes package name
     */
    public void setOutputPackagePrefix(String packagePrefix) {
        if (packagePrefix == null || packagePrefix.length() == 0) {
            this.packagePrefix = null;
            return;
        }
        Matcher matcher = PACKAGE_NAME_PATTERN.matcher(packagePrefix);
        if (matcher.matches()) {
            this.packagePrefix = packagePrefix;
        } else {
            throw (new IllegalArgumentException("The name " + packagePrefix
                    + " is not a valid java package prefix name"));
        }

    }

    /**
     * @param configFilePath the path to the optional configuration file
     */
    public void setConfigProps(final String configFilePath) {
        if (configFilePath == null) {
            this.configProps = null;
            return;
        }
        File configFile = new File(configFilePath);
        if (configFile.exists()) {
            if (configFile.isDirectory()) {
                throw new IllegalArgumentException("Folder '" + configFilePath
                        + "' cannot be used as a configuration file");
            }

            FileInputStream fis = null;
            try {
                fis = new FileInputStream(configFile);
                this.configProps = new Properties();
                configProps.load(fis);
            } catch (IOException e) {
                throw new IllegalArgumentException("Configuration file '"
                        + configFilePath + "' is not valid");
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        log.error("Unable to close configuration file", e);
                    }
                }
            }
        } else {
            throw new IllegalArgumentException("Configuration file '"
                    + configFilePath + "' not found");
        }
    }

    public void setXsltFileName(String xsltFileName) {
        this.xsltFileName = xsltFileName;
    }

    public File getInput() {
        return input;
    }

    public String getInputEncoding() {
        return inputEncoding;
    }

    public File getOutput() {
        return output;
    }

    public String getOutputPackage() {
        return packagePrefix;
    }

    public Properties getConfigProps() {
        return configProps;
    }

    public String getXsltFileName() {
        return xsltFileName;
    }

}
