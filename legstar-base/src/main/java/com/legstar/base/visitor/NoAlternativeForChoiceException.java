package com.legstar.base.visitor;

/**
 * No alternatives exist for a Choice. One of the alternatives must exist.
 * 
 */
public class NoAlternativeForChoiceException extends RuntimeException {

    private static final long serialVersionUID = 6086957263537052911L;

    public NoAlternativeForChoiceException(String choiceTypeName) {
        super("No alternative was found for the " + choiceTypeName + " choice");
    }

}
