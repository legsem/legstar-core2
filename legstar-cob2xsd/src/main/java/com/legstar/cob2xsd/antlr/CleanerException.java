/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd.antlr;

/**
 * Something wrong with the source (null, unreadable, empty, ...).
 *
 */
public class CleanerException extends RecognizerException {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -110771617742885020L;

    /**
     * @param msg the exception message
     */
    public CleanerException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public CleanerException(final Throwable e) {
        super(e);
    }
}
