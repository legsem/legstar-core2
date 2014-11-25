package com.legsem.legstar.visitor;

import java.util.Map;
import java.util.Map.Entry;

import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.FromHostException;
import com.legsem.legstar.type.composite.CobolArrayType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolPrimitiveType;

/**
 * Default strategy for choice alternative selection.
 * <p/>
 * Alternatives are taken in the order they were added to the Choice. The first
 * one that validates (mainframe data is compatible) is selected.
 * <p/>
 * If no alternative validates, returns null.
 */
public class DefaultFromCobolChoiceStrategy implements FromCobolChoiceStrategy {

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

        ValidateFromCobolVisitor visitor = new ValidateFromCobolVisitor(
                hostData, start);
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

}
