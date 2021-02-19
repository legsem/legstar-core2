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
 * Clean a free format COBOL source.
 * <p>
 * Free format is the Microfocus and open COBOL format where column 1 is the
 * indicator area but code can start at column 1 as well. There are no area A
 * and B anymore and code has no fixed right margin.
 * 
 */
public class CobolFreeFormatSourceCleaner extends AbstractCobolSourceCleaner {

    /**
     * Construct with a shared error handler.
     * 
     * @param errorHandler handles error messages
     */
    public CobolFreeFormatSourceCleaner(
            final RecognizerErrorHandler errorHandler) {
        super(errorHandler);

    }

    /** {@inheritDoc} */
    @Override
    public int getIndicatorAreaPos() {
        return 0;
    }

    @Override
    public boolean isComment(String line) {
        // If first non white space character on the line is a comment indicator
        char indicatorArea = line.trim().charAt(0);
        if (indicatorArea == '*' || indicatorArea == '/'
                || indicatorArea == '$') {
            return true;
        }
        return false;
    }

}
