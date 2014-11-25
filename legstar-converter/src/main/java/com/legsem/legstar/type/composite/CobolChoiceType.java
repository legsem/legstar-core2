package com.legsem.legstar.type.composite;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.visitor.CobolVisitor;

/**
 * A COBOL REDEFINES clause translates into a choice between alternatives (which
 * could be primitive or composite types).
 * 
 */
public class CobolChoiceType extends CobolCompositeType {

    /**
     * Alternatives are kept in a LinkedHashMap to preserve entry order.
     */
    private final LinkedHashMap < String, CobolType > alternatives;

    /**
     * Allows to efficiently retrieve the name of an alternative in its parent
     * choice.
     */
    private final Map < CobolType, String > namesMap;

    public CobolChoiceType(CobolContext cobolContext,
            LinkedHashMap < String, CobolType > alternatives) {
        super(cobolContext);
        this.alternatives = alternatives;
        this.namesMap = new HashMap < CobolType, String >();
        for (Entry < String, CobolType > entry : alternatives.entrySet()) {
            namesMap.put(entry.getValue(), entry.getKey());
        }
    }

    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public LinkedHashMap < String, CobolType > getAlternatives() {
        return alternatives;
    }

    /**
     * Map an alternative to its in the parent choice
     * 
     * @return Map of alternative to its in the parent choice
     */
    public Map < CobolType, String > getNamesMap() {
        return namesMap;
    }

}
