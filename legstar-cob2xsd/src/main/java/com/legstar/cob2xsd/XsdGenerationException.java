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
package com.legstar.cob2xsd;

/**
 * The generation of the XML schema failed.
 *
 */
public class XsdGenerationException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = 7160263971438334834L;

    /**
     * @param msg the exception message
     */
    public XsdGenerationException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public XsdGenerationException(final Throwable e) {
        super(e);
    }
}
