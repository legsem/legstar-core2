package com.legstar.base.type.primitive;

import com.legstar.base.FromHostException;
import com.legstar.base.FromHostResult;
import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolType;
import com.legstar.base.visitor.CobolVisitor;

/**
 * A primitive COBOL type.
 * <p/>
 * Because primitive types tend to have lots of optional properties they are
 * created using Builders.
 * 
 * @param <T> the underlying java type
 */
public abstract class CobolPrimitiveType<T> extends CobolType {

    /**
     * The target java type.
     */
    private final Class < T > javaClass;

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
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid type
     */
    public boolean isValid(CobolContext cobolContext, byte[] hostData, int start) {
        return isValid(javaClass, cobolContext, hostData, start);
    }

    /**
     * Check if a byte array contains valid mainframe data for this type
     * characteristics.
     * 
     * @param javaClass target java type
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return true if the byte array contains a valid type
     */
    public abstract boolean isValid(Class < T > javaClass, CobolContext cobolContext, byte[] hostData, int start);

    /**
     * Convert mainframe data into a Java object.
     * 
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the mainframe value as a java object
     * @throws FromHostException if conversion fails
     */
    public FromHostResult < T > fromHost(CobolContext cobolContext, byte[] hostData, int start)
            throws FromHostException {
        return fromHost(javaClass, cobolContext, hostData, start);
    }

    /**
     * Convert mainframe data into a Java object.
     * 
     * @param javaClass target java type
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the mainframe value as a java object
     * @throws FromHostException if conversion fails
     */
    public abstract FromHostResult < T > fromHost(Class < T > javaClass, CobolContext cobolContext, byte[] hostData, int start)
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

    public Class < T > getJavaClass() {
        return javaClass;
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
        protected final Class < T > javaClass;

        // Optional
        protected boolean odoObject = false;
        protected boolean customVariable = false;

        public Builder(Class < T > javaClass) {
            this.javaClass = javaClass;
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
        javaClass = builder.javaClass;
        odoObject = builder.odoObject;
        customVariable = builder.customVariable;

    }

}
