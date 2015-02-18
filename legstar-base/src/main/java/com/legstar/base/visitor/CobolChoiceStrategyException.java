package com.legstar.base.visitor;

/**
 * A failure while trying to select an alternative for a choice.
 *
 */
public class CobolChoiceStrategyException extends RuntimeException {

    private static final long serialVersionUID = -3752509461849543748L;

    public CobolChoiceStrategyException(String message, Throwable cause) {
        super(message, cause);
    }

    public CobolChoiceStrategyException(String message) {
        super(message);
    }

}
