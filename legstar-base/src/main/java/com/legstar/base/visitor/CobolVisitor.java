package com.legstar.base.visitor;

import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolPrimitiveType;

/**
 * Visitor walk a COBOL complex structure described by linked
 * {@link com.legstar.base.type.CobolType} in a depth-first way (visit child
 * before sibling).
 * <p/>
 * Visitors are mutable objects keeping contextual data during traversal. They
 * are not thread safe.
 * 
 */
public interface CobolVisitor {

    void visit(CobolComplexType type);

    void visit(CobolArrayType type);

    void visit(CobolChoiceType type);

    void visit(CobolPrimitiveType < ? > type);

}
