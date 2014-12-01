package com.legstar.base.generator;


/**
 * Failed to generate java converter classes from an XML schema.
 *
 */
public class Xsd2ConverterException extends RuntimeException {

    private static final long serialVersionUID = -7657561588025029061L;

    public Xsd2ConverterException(final String message) {
        super(message);
    }

    public Xsd2ConverterException(final String message, Throwable cause) {
        super(message, cause);
    }

    public Xsd2ConverterException(Throwable cause) {
        super(cause);
    }
}
