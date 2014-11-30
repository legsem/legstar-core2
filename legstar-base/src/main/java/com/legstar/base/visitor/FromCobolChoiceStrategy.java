package com.legstar.base.visitor;

import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolChoiceType;

public interface FromCobolChoiceStrategy {
    
    CobolType choose(String choiceFieldName, CobolChoiceType choiceType, Map < String, Object > variables, byte[] hostData, int start);

}
