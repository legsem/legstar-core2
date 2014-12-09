package com.legstar.jaxb.converter;

import com.legstar.base.FromHostResult;
import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.MaxBytesLenCobolVisitor;
import com.legstar.jaxb.converter.Cob2JaxbVisitor;
import com.legstar.jaxb.converter.JaxbWrapper;

/**
 * Host to JAXB converter.
 * <p/>
 * Given a buffer of Host data corresponding to a COBOL copybook.
 * <p/>
 * Uses a {@link CobolComplexType} to visit all fields of the COBOL copybook.
 * <p/>
 * Uses a {@link JaxbWrapperFactory} to create resulting JAXB instances.
 *
 * @param <J> the target JAXB object type
 */
public class Cob2Jaxb<J> {

    /**
     * Parameters such as host character set.
     */
    private final CobolContext cobolContext;

    /**
     * The COBOL type mapping to the target JAXB object.
     */
    private final CobolComplexType cobolComplexType;

    /**
     * Provides methods to create and populate the target JAXB object
     */
    private final JaxbWrapperFactory jaxbWrapperFactory;

    /**
     * The target JAXB instance class
     */
    private final Class < J > jaxbClass;

    public Cob2Jaxb(CobolComplexType cobolComplexType,
            JaxbWrapperFactory jaxbWrapperFactory, Class < J > jaxbClass) {
        this(new EbcdicCobolContext(), cobolComplexType, jaxbWrapperFactory,
                jaxbClass);
    }

    /**
     * Create a converter.
     * 
     * @param cobolContext holds parameters such as the host character set
     *            (usually EBCDIC)
     * @param cobolComplexType the COBOL complex type mapping to the target JAXB
     *            object (usually produced by the legstar JAXB generator)
     * @param jaxbWrapperFactory provides methods to create and populate the
     *            target JAXB object (usually produced by the legstar JAXB
     *            generator)
     * @param jaxbClass the target JAXB instance class. The JAXB class is
     *            produced by the legstar JAXB generator.
     */
    public Cob2Jaxb(CobolContext cobolContext,
            CobolComplexType cobolComplexType,
            JaxbWrapperFactory jaxbWrapperFactory, Class < J > jaxbClass) {
        this.cobolContext = cobolContext;
        this.cobolComplexType = cobolComplexType;
        this.jaxbWrapperFactory = jaxbWrapperFactory;
        this.jaxbClass = jaxbClass;
    }

    /**
     * Convert host data to a JAXB instance.
     * <p/>
     * 
     * @param hostData the buffer of host data
     * @param start where to start in the buffer of host data
     * @return a result object containing the JAXB instance created as well as
     *         the total number of host bytes used to generate that JAXB
     *         instance
     */
    public FromHostResult < J > convert(byte[] hostData, int start) {
        Cob2JaxbVisitor visitor = new Cob2JaxbVisitor(cobolContext, hostData,
                start, jaxbWrapperFactory);
        visitor.visit(cobolComplexType);
        JaxbWrapper < ? > jaxbWrapper = visitor
                .getLastObject(JaxbWrapper.class);
        return new FromHostResult < J >(visitor.getLastPos(),
                jaxbClass.cast(jaxbWrapper.getJaxb()));
    }

    /**
     * Evaluates the maximum number of bytes for host data corresponding to the
     * COBOL type converted.
     * 
     * @return the maximum number of bytes for host data corresponding to the
     *         COBOL type converted
     */
    public int getMaxBytesLen() {
        MaxBytesLenCobolVisitor maxLenVisitor = new MaxBytesLenCobolVisitor();
        maxLenVisitor.visit(cobolComplexType);
        return maxLenVisitor.getMaxBytesLen();
    }

}
