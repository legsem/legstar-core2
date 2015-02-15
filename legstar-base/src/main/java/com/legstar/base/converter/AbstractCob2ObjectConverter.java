package com.legstar.base.converter;

import java.util.Set;

import com.legstar.base.FromHostResult;
import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.FromCobolChoiceStrategy;

/**
 * Converts a mainframe datum to an Object.
 * <p/>
 * Implementation classes must be immutable and Thread safe.
 *
 * @param <T> the type of Object produced
 */
public abstract class AbstractCob2ObjectConverter<T> implements
        Cob2ObjectConverter < T > {

    /**
     * Input COBOL type mapping to the target hash map.
     */
    private final CobolComplexType cobolComplexType;

    /**
     * Parameters such as host character set.
     */
    private final CobolContext cobolContext;

    /**
     * Optionally, user might provide a custom strategy for alternative
     * selection.
     */
    private final FromCobolChoiceStrategy customChoiceStrategy;

    /**
     * Optionally, user might require variable values to be collected during
     * visiting.
     */
    private final Set < String > customVariables;

    public FromHostResult < T > convert(byte[] hostData) {
        return convert(hostData, 0);
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public abstract static class Builder<T, B extends Builder < T, B >> {

        private CobolComplexType cobolComplexType;
        private CobolContext cobolContext;
        private FromCobolChoiceStrategy customChoiceStrategy;
        private Set < String > customVariables;

        public B cobolComplexType(CobolComplexType cobolComplexType) {
            this.cobolComplexType = cobolComplexType;
            return self();
        }

        public B cobolContext(CobolContext cobolContext) {
            this.cobolContext = cobolContext;
            return self();
        }

        public B customChoiceStrategy(
                FromCobolChoiceStrategy customChoiceStrategy) {
            this.customChoiceStrategy = customChoiceStrategy;
            return self();
        }

        public B customVariables(Set < String > customVariables) {
            this.customVariables = customVariables;
            return self();
        }

        protected abstract B self();

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    public AbstractCob2ObjectConverter(Builder < T, ? > builder) {
        cobolComplexType = builder.cobolComplexType;
        cobolContext = builder.cobolContext == null ? new EbcdicCobolContext()
                : builder.cobolContext;
        customChoiceStrategy = builder.customChoiceStrategy;
        customVariables = builder.customVariables;
        if (cobolComplexType == null) {
            throw new IllegalArgumentException(
                    "You must provide a valid input CobolComplexType");
        }
    }

    // -----------------------------------------------------------------------------
    // Getters
    // -----------------------------------------------------------------------------
    public CobolComplexType getCobolComplexType() {
        return cobolComplexType;
    }

    public CobolContext getCobolContext() {
        return cobolContext;
    }

    public FromCobolChoiceStrategy getCustomChoiceStrategy() {
        return customChoiceStrategy;
    }

    public Set < String > getCustomVariables() {
        return customVariables;
    }

}
