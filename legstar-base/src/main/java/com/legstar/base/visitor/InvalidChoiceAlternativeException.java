package com.legstar.base.visitor;

/**
 * An alternative type does not correspond to a given Choice.
 * 
 */
public class InvalidChoiceAlternativeException extends RuntimeException {

    private static final long serialVersionUID = -3298972488220426263L;

    public InvalidChoiceAlternativeException(String choiceTypeName,
            int alternativeIndex) {
        super("Index " + alternativeIndex
                + " is not a valid alternative index for the " + choiceTypeName
                + " choice");
    }

}
