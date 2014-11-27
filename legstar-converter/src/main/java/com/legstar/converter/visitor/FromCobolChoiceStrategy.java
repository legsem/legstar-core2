package com.legstar.converter.visitor;

import java.util.Map;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.type.composite.CobolChoiceType;

public interface FromCobolChoiceStrategy {
    
    CobolType choose(String choiceFieldName, CobolChoiceType choiceType, Map < String, Object > variables, byte[] hostData, int start);

}
