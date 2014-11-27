package com.legstar.converter.visitor;

import java.util.Map;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;
import com.legstar.converter.visitor.FromCobolChoiceStrategy;

/**
 * Example of a custom strategy.
 * 
 */
public class Rdef03ObjectFromHostChoiceStrategy implements
        FromCobolChoiceStrategy {

    public CobolType choose(String choiceFieldName, CobolChoiceType choiceType,
            Map < String, Object > variables, byte[] hostData, int start) {

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

}
