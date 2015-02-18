package com.legstar.base.type.primitive;

import com.legstar.base.context.CobolContext;
import com.legstar.base.type.CobolOptionalType;
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
public abstract class CobolPrimitiveType<T> implements CobolType,
        CobolOptionalType {

    /**
     * The target java type.
     */
    private final Class < T > javaClass;

    /**
     * The original COBOL name of this type.
     */
    private final String cobolName;

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
     * Optional types may depend on some other field.
     */
    private final String dependingOn;

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
    public abstract boolean isValid(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start);

    /**
     * Convert mainframe data into a Java object.
     * 
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the mainframe value as a java object
     */
    public FromHostPrimitiveResult < T > fromHost(CobolContext cobolContext,
            byte[] hostData, int start) {
        return fromHost(javaClass, cobolContext, hostData, start);
    }

    /** {@inheritDoc} */
    public long getMaxBytesLen() {
        return getBytesLen();
    }

    /** {@inheritDoc} */
    public long getMinBytesLen() {
        return getBytesLen();
    }

    /**
     * Determine the length in bytes of the mainframe representation of this
     * type.
     * 
     * @return the size in bytes needed to store this type
     */
    public abstract int getBytesLen();

    /**
     * Convert mainframe data into a Java object.
     * 
     * @param javaClass target java type
     * @param cobolContext host COBOL configuration parameters
     * @param hostData the byte array containing mainframe data
     * @param start the start position for the expected type in the byte array
     * @return the conversion results, including the mainframe value as a java
     *         object if conversion succeeded
     */
    public abstract FromHostPrimitiveResult < T > fromHost(Class < T > javaClass,
            CobolContext cobolContext, byte[] hostData, int start);

    /** {@inheritDoc} */
    public void accept(CobolVisitor visitor) {
        visitor.visit(this);
    }

    public Class < T > getJavaClass() {
        return javaClass;
    }

    public String getCobolName() {
        return cobolName;
    }

    public boolean isOdoObject() {
        return odoObject;
    }

    public boolean isCustomVariable() {
        return customVariable;
    }

    public String getDependingOn() {
        return dependingOn;
    }

    // -----------------------------------------------------------------------------
    // Builder section
    // -----------------------------------------------------------------------------
    public abstract static class Builder<T, B extends Builder < T, B >> {

        // Required
        protected final Class < T > javaClass;

        // Optional
        protected String cobolName;
        protected boolean odoObject = false;
        protected boolean customVariable = false;
        private String dependingOn;

        public Builder(Class < T > javaClass) {
            this.javaClass = javaClass;
        }

        public B cobolName(String value) {
            cobolName = value;
            return self();
        }

        public B odoObject(boolean value) {
            odoObject = value;
            return self();
        }

        public B customVariable(boolean value) {
            customVariable = value;
            return self();
        }

        public B dependingOn(String dependingOn) {
            this.dependingOn = dependingOn;
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
        dependingOn = builder.dependingOn;
        customVariable = builder.customVariable;
        if (builder.cobolName == null) {
            throw new IllegalArgumentException(
                    "You must provide a COBOL name for this type");
        }
        cobolName = builder.cobolName;
    }

}
