package com.legstar.converter.type;

/**
 * Some anomaly at runtime when trying to convert to or from mainframe data.
 *
 */
public class ConversionException extends RuntimeException {

    private static final long serialVersionUID = 7300869847784507910L;

    public ConversionException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConversionException(String message) {
        super(message);
    }

}
