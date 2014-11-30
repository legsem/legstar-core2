/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd.antlr;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.stringtemplate.StringTemplate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.legstar.cobol.CobolStructureParser;


/**
 * Not a real test. A convenience class that generates a reference graph for
 * documentation purposes. The Graph is an abstract syntax tree representing
 * mainframe data.
 *
 */
public class ASTReferenceGraph {
    
    /** Logger. */
    private static final Logger _log = LoggerFactory.getLogger(ASTReferenceGraph.class);

    /**
     * Produce a graph of all possible attributes.
     */
    public void testGraphProd() {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        CommonTree ast = new CommonTree();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
 
        /*
         * Data description entries.
         */
        Object dataItem = adaptor.create(CobolStructureParser.DATA_ITEM, "DATA_ITEM");
        addAttribute(adaptor, dataItem, CobolStructureParser.LEVEL, "LEVEL", "level");
        addAttribute(adaptor, dataItem, CobolStructureParser.NAME, "NAME", "name | FILLER");
        addAttribute(adaptor, dataItem, CobolStructureParser.REDEFINES, "REDEFINES", "dataItemName");

        /* Usage keyword */
        addAttribute(adaptor, dataItem, CobolStructureParser.USAGE, "USAGE",
                "BINARY | SINGLEFLOAT | DOUBLEFLOAT | PACKEDDECIMAL | NATIVEBINARY | DISPLAY | DISPLAY1"
                + " | INDEX | NATIONAL | POINTER | PROCEDUREPOINTER | FUNCTIONPOINTER");

        /* Picture keyword */
        addAttribute(adaptor, dataItem, CobolStructureParser.PICTURE, "PICTURE", "pictureSymbols");
        
        /* Value keyword */
        addAttribute(adaptor, dataItem, CobolStructureParser.VALUE, "VALUE"
                , "INT | SIGNED_INT | FLOAT_LITERAL | DECIMAL_LITERAL"
                + " | ALPHANUM_LITERAL_STRING | HEX_LITERAL_STRING | ZERO_LITERAL_STRING"
                + " | DBCS_LITERAL_STRING | NATIONAL_LITERAL_STRING | NATIONAL_HEX_LITERAL_STRING"
                + " | ZERO_CONSTANT | SPACE_CONSTANT | HIGH_VALUE_CONSTANT | LOW_VALUE_CONSTANT"
                + " | QUOTE_CONSTANT | ALL_CONSTANT | NULL_CONSTANT");

        /* Date format clause */
        addAttribute(adaptor, dataItem, CobolStructureParser.DATEFORMAT, "DATEFORMAT",
                "datePattern");

        /* Array types */
        Object fixedArrayNode = addAttribute(adaptor, dataItem, CobolStructureParser.FIXEDARRAY, "FIXEDARRAY", null);
        addAttribute(adaptor, fixedArrayNode, CobolStructureParser.HBOUND, "HBOUND", "highBound");
        addAttribute(adaptor, fixedArrayNode, CobolStructureParser.KEY, "KEY", "dataItemName");
        addAttribute(adaptor, fixedArrayNode, CobolStructureParser.INDEX, "INDEX", "name");
 
        Object variableArrayNode = addAttribute(adaptor, dataItem, CobolStructureParser.VARARRAY, "VARARRAY", null);
        addAttribute(adaptor, variableArrayNode, CobolStructureParser.LBOUND, "LBOUND", "lowBound");
        Object highBoundNode = addAttribute(
                adaptor, variableArrayNode, CobolStructureParser.HBOUND, "HBOUND", "highBound");
        addAttribute(adaptor, highBoundNode, CobolStructureParser.DEPENDINGON, "DEPENDINGON", "dataItemName");
        addAttribute(adaptor, variableArrayNode, CobolStructureParser.KEY, "KEY", "name");
        addAttribute(adaptor, variableArrayNode, CobolStructureParser.INDEX, "INDEX", "name");
 
        /* Sign clause elementary types */
        Object signNode = addAttribute(adaptor, dataItem, CobolStructureParser.SIGN, "SIGN", null);
        Object leadingNode = addAttribute(adaptor, signNode, CobolStructureParser.LEADING, "LEADING", null);
        addAttribute(adaptor, leadingNode, CobolStructureParser.SEPARATE, "SEPARATE", null);
        Object trailingNode = addAttribute(adaptor, signNode, CobolStructureParser.TRAILING, "TRAILING", null);
        addAttribute(adaptor, trailingNode, CobolStructureParser.SEPARATE, "SEPARATE", null);
        
        /* Other attributes */
        addAttribute(adaptor, dataItem, CobolStructureParser.BLANKWHENZERO, "BLANKWHENZERO", null);
        addAttribute(adaptor, dataItem, CobolStructureParser.EXTERNAL, "EXTERNAL", null);
        addAttribute(adaptor, dataItem, CobolStructureParser.GLOBAL, "GLOBAL", null);
        addAttribute(adaptor, dataItem, CobolStructureParser.GROUPUSAGENATIONAL, "GROUPUSAGENATIONAL", null);
        addAttribute(adaptor, dataItem, CobolStructureParser.JUSTIFIEDRIGHT, "JUSTIFIEDRIGHT", null);
        addAttribute(adaptor, dataItem, CobolStructureParser.SYNCHRONIZED, "SYNCHRONIZED",
                "LEFT | RIGHT");

        adaptor.addChild(ast, dataItem);

        
        /*
         * Rename entries.
         */
        Object renameItem = adaptor.create(CobolStructureParser.RENAME, "RENAME");
        addAttribute(adaptor, renameItem, CobolStructureParser.LEVEL, "LEVEL", "66");
        addAttribute(adaptor, renameItem, CobolStructureParser.NAME, "NAME", "name");
        addAttribute(adaptor, renameItem, CobolStructureParser.RANGE, "RANGE", "fromDataItemName | toDataItemName");
        addAttribute(adaptor, renameItem, CobolStructureParser.LITERAL, "LITERAL", "dataItemName");
        
        adaptor.addChild(ast, renameItem);
        
        /*
         * Condition entries.
         */
        Object conditionItem = adaptor.create(CobolStructureParser.CONDITION, "CONDITION");
        addAttribute(adaptor, conditionItem, CobolStructureParser.LEVEL, "LEVEL", "88");
        addAttribute(adaptor, conditionItem, CobolStructureParser.NAME, "NAME", "name");
        addAttribute(adaptor, conditionItem, CobolStructureParser.RANGE, "RANGE", "fromValue | toValue");
        addAttribute(adaptor, conditionItem, CobolStructureParser.LITERAL, "LITERAL", "value");
        
        adaptor.addChild(ast, conditionItem);
        
        
        StringTemplate st = gen.toDOT(ast);
        _log.info(st.toString());
    }
    
    /**
     * Add a new attribute to a parent node.
     * @param adaptor the tree helper
     * @param parent parent node
     * @param tokenType the new node token type
     * @param tokenText the new node name
     * @param value the new node value
     * @return a new node that was just added to parent
     */
    private Object addAttribute(
            final TreeAdaptor adaptor,
            final Object parent,
            final int tokenType,
            final String tokenText, final String value) {
        Object node = adaptor.create(tokenType, tokenText);
        if (value != null) {
            String[] values = value.split("\\s\\|\\s");
            for (String avalue :  values) {
                Object nodeValue = adaptor.create(CobolStructureParser.LITERAL, avalue);
                adaptor.addChild(node, nodeValue);
            }
        }
        adaptor.addChild(parent, node);
        return node;
        
    }
    

}
