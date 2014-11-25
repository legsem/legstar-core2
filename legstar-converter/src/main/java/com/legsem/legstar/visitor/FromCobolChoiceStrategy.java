package com.legsem.legstar.visitor;

import java.util.Map;

import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.composite.CobolChoiceType;

public interface FromCobolChoiceStrategy {
    
    CobolType choose(String choiceFieldName, CobolChoiceType choiceType, Map < String, Object > variables, byte[] hostData, int start);

}
