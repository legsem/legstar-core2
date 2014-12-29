package com.legstar.base.visitor;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.legstar.base.FromHostException;
import com.legstar.base.context.CobolContext;
import com.legstar.base.converter.Cob2ObjectValidator;
import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

/**
 * Default strategy for choice alternative selection.
 * <p/>
 * Alternatives are taken in the order they were added to the Choice. The first
 * one that validates (mainframe data is compatible) is selected.
 * <p/>
 * If no alternative validates, returns null.
 */
public class DefaultFromCobolChoiceStrategy implements FromCobolChoiceStrategy {

    /**
     * Host COBOL configuration parameters.
     */
    private final CobolContext cobolContext;

    public DefaultFromCobolChoiceStrategy(CobolContext cobolContext) {
        this.cobolContext = cobolContext;
    }

    public CobolType choose(String choiceFieldName,
            CobolChoiceType cobolChoiceType, Map < String, Object > variables,
            byte[] hostData, int start) {

        for (Entry < String, CobolType > alternative : cobolChoiceType
                .getAlternatives().entrySet()) {

            if (tryAlternative(choiceFieldName, alternative.getValue(),
                    alternative.getKey(), hostData, start)) {
                return alternative.getValue();
            }
        }

        return null;
    }

    private boolean tryAlternative(String choiceFieldName,
            CobolType alternative, String alternativeName, byte[] hostData,
            int start) {

        Cob2ObjectValidator visitor = new Cob2ObjectValidator(
                cobolContext, hostData, start);
        if (alternative instanceof CobolComplexType) {
            visitor.visit((CobolComplexType) alternative);
        } else if (alternative instanceof CobolArrayType) {
            visitor.visit((CobolArrayType) alternative);
        } else if (alternative instanceof CobolPrimitiveType) {
            visitor.visit((CobolPrimitiveType < ? >) alternative);
        } else {
            throw new FromHostException("Invalid alternative "
                    + alternativeName + " for choice field " + choiceFieldName,
                    hostData, start);
        }
        return visitor.isValid();

    }

    public Set < String > getVariableNames() {
        return null;
    }

}
