package com.legstar.converter.generator;


public class CobXsd2ConverterException extends RuntimeException {

    private static final long serialVersionUID = -7657561588025029061L;

    public CobXsd2ConverterException(final String message) {
        super(message);
    }

    public CobXsd2ConverterException(final String message, Throwable cause) {
        super(message, cause);
    }

    public CobXsd2ConverterException(Throwable cause) {
        super(cause);
    }
}
