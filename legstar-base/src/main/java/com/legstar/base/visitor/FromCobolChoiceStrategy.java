package com.legstar.base.visitor;

import java.util.Map;
import java.util.Set;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;

public interface FromCobolChoiceStrategy {

    /**
     * Select one alternative in choices.
     * 
     * @param choiceFieldName the choice field name in the parent structure.
     * @param choiceType the choice field type
     * @param variables contains all custom variables values collected before
     *            reaching the choice
     * @param hostData the host data
     * @param start the start position within the host data for the entire
     *            parent structure
     * @return a selected alternative or null if unable to select one
     */
    CobolType choose(String choiceFieldName, CobolChoiceType choiceType,
            Map < String, Object > variables, byte[] hostData, int start);

    /**
     * Provides a list of variable names whose values are to be collected before
     * the choice is reached. These values are needed to make the alternative
     * choice decision.
     * 
     * @return
     */
    Set < String > getVariableNames();

}
