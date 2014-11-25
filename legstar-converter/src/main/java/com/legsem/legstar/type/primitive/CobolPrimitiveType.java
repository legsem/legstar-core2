package com.legsem.legstar.type.primitive;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.FromHostException;
import com.legsem.legstar.type.FromHostResult;
import com.legsem.legstar.visitor.CobolVisitor;

/**
 * A primitive COBOL type.
 * <p/>
 * Because primitive type tends to have lots of optional properties they are
 * created using Builders.
 * 
 * @param <T> the underlying java type
 */
public abstract class CobolPrimitiveType<T> extends CobolType {

    /**
     * This type's value gives the runtime size of a variable size array.
     */
    private final boolean odoObject;

    /**
     * This type has been marked by user as a custom variable (whose value is
     * needed by custom code somewhere).
     */
    private final boolean customVariable;

    /**
     * Check if a byte array contains valid mainframe data for this type
     * characteristics.
     * 
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid type
     */
    public abstract boolean isValid(byte[] hostData, int start);

    /**
     * Convert mainframe data into a Java object.
     * 
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the mainframe value as a java object
     * @throws FromHostException if conversion fails
     */
    public abstract FromHostResult < T > fromHost(byte[] hostData, int start)
            throws FromHostException;

    /**
     * Determine the length in bytes of the mainframe representation of this
     * type.
     * 
     * @return the size in bytes needed to store this type
     */
    public abstract int getBytesLen();

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public boolean isOdoObject() {
        return odoObject;
    }

    public boolean isCustomVariable() {
        return customVariable;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public abstract static class Builder<T, B extends Builder < T, B >> {

        // Required
        private final CobolContext cobolContext;

        // Optional
        private boolean odoObject = false;
        private boolean customVariable = false;

        public Builder(CobolContext cobolContext) {
            this.cobolContext = cobolContext;
        }

        public B odoObject(boolean value) {
            odoObject = value;
            return self();
        }

        public B customVariable(boolean value) {
            customVariable = value;
            return self();
        }

        protected abstract B self();

    }

    // -----------------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------------
    public CobolPrimitiveType(Builder < T, ? > builder) {
        super(builder.cobolContext);

        odoObject = builder.odoObject;
        customVariable = builder.customVariable;

    }

}
