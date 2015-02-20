package com.legstar.base.visitor;

import com.legstar.base.type.CobolType;

/**
 * Conversion from host data to java failure.
 *
 */
public class FromCobolException extends RuntimeException {

    private static final long serialVersionUID = 6371078862061962390L;

    public FromCobolException(String message, String cobolFullName,
            CobolType type) {
        super(formatMessage(message, cobolFullName, type));
    }

    public FromCobolException(String message, String cobolFullName,
            CobolType type, Throwable exception) {
        super(formatMessage(message, cobolFullName, type), exception);
    }

    private static String formatMessage(String message, String cobolFullName,
            CobolType type) {
        return String.format("%s. COBOL variable path: %s. COBOL type: %s", message,
                cobolFullName, type.getClass().getSimpleName());
    }

}
