package com.legstar.base.visitor;

/**
 * Unable to locate a valid counter for an ODO array.
 *
 */
public class CobolODOResolutionException extends RuntimeException {

    private static final long serialVersionUID = -6737906782757119853L;

    public CobolODOResolutionException(String message, Throwable cause) {
        super(message, cause);
    }

    public CobolODOResolutionException(String message) {
        super(message);
    }

}
