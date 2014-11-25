package com.legsem.legstar.visitor;

import com.legsem.legstar.type.ConversionException;
import com.legsem.legstar.type.composite.CobolArrayType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolPrimitiveType;

/**
 * Visitor walk a COBOL complex structure described by linked {@link CobolType}
 * in a depth-first way (visit child before sibling).
 * <p/>
 * Visitors are mutable objects keeping contextual data during traversal. They
 * are not thread safe.
 * 
 */
public interface CobolVisitor {

    void visit(CobolComplexType type) throws ConversionException;

    void visit(CobolArrayType type) throws ConversionException;

    void visit(CobolChoiceType type) throws ConversionException;

    void visit(CobolPrimitiveType < ? > type) throws ConversionException;

}
