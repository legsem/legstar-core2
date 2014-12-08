package com.legstar.jaxb.generator;


/**
 * Failed to generate java converter classes from a COBOL source.
 *
 */
public class Cob2JaxbGeneratorException extends RuntimeException {

    private static final long serialVersionUID = -725389126361952370L;

    public Cob2JaxbGeneratorException(final String message) {
        super(message);
    }

    public Cob2JaxbGeneratorException(final String message, Throwable cause) {
        super(message, cause);
    }

    public Cob2JaxbGeneratorException(Throwable cause) {
        super(cause);
    }
}
