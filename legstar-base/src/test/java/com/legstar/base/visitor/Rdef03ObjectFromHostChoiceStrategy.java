package com.legstar.base.visitor;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.visitor.FromCobolChoiceStrategy;

/**
 * Example of a custom strategy.
 * 
 */
public class Rdef03ObjectFromHostChoiceStrategy implements
        FromCobolChoiceStrategy {

    public CobolType choose(String choiceFieldName, CobolChoiceType choiceType,
            Map < String, Object > variables, byte[] hostData, int start, int length) {

        int select = ((Number) variables.get("comSelect")).intValue();

        switch (select) {
        case 0:
            return choiceType.getAlternatives().get("comDetail1");
        case 1:
            return choiceType.getAlternatives().get("comDetail2");
        case 2:
            return choiceType.getAlternatives().get("comDetail3");
        default:
            return null;

        }
    }

    public Set < String > getVariableNames() {
        Set <String> varNames = new HashSet < String >();
        varNames.add("comSelect");
        return varNames;
    }

}
