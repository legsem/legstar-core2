package com.legstar.base.visitor;

/**
 * An alternative type does not correspond to a given Choice.
 * 
 */
public class InvalidChoiceTypeAlternative extends RuntimeException {

    private static final long serialVersionUID = -3298972488220426263L;

    public InvalidChoiceTypeAlternative(String choiceTypeName, Class <?> alternativeType) {
        super("Type " + alternativeType + " is not an alternative of the "
                + choiceTypeName + " choice");
    }

}
