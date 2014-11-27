package com.legstar.converter.type.composite;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.legstar.converter.type.CobolType;
import com.legstar.converter.visitor.CobolVisitor;

/**
 * A COBOL REDEFINES clause translates into a choice between alternatives (which
 * could be primitive or composite types).
 * 
 */
public class CobolChoiceType extends CobolCompositeType {

    /**
     * List of alternatives mapping to their COBOL type.
     */
    private final Map < String, CobolType > alternatives;

    /**
     * Allows to efficiently retrieve the name of an alternative given its COBOL
     * type.
     */
    private final Map < CobolType, String > namesMap;

    public CobolChoiceType(Map < String, CobolType > alternatives) {
        this.alternatives = alternatives;
        this.namesMap = new HashMap < CobolType, String >();
        for (Entry < String, CobolType > entry : alternatives.entrySet()) {
            namesMap.put(entry.getValue(), entry.getKey());
        }
    }

    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public Map < String, CobolType > getAlternatives() {
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
