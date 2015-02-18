package com.legstar.base.visitor;

public class FromCobolException extends RuntimeException {

    private static final long serialVersionUID = 6371078862061962390L;
    
    public FromCobolException(String message, String cobolFullName) {
        super(formatMessage(message, cobolFullName));
    }
    
    public FromCobolException(String message, String cobolFullName, Throwable exception) {
        super(formatMessage(message, cobolFullName), exception);
    }
    
    private static String formatMessage(String message, String cobolFullName) {
        return String.format("%s. COBOL variable path: %s.", message, cobolFullName);
    }

}
