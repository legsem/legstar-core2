/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
tree grammar CobolStructureEmitter;
/*------------------------------------------------------------------
 * Generates a COBOL model from an AST.
 *------------------------------------------------------------------*/
options {
    tokenVocab = CobolStructureParser;
    ASTLabelType=CommonTree;
}

/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.cobol;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.cobol.model.CobolUsage.Usage;
}

/*------------------------------------------------------------------
 * Emitter grammar. Populates a list of data entries
 *------------------------------------------------------------------*/
cobdata[List < CobolDataItem > dataEntries]
     :  data_entry[$dataEntries]*
     ;
    
data_entry[List < CobolDataItem > dataEntries]
scope {
    CobolDataItem dataEntry;
}
@init {
    $data_entry::dataEntry = new CobolDataItem();
    $data_entry::dataEntry.setSrceLine(((CommonTree) input.LT(1)).getLine());
}
@after {
    $dataEntries.add($data_entry::dataEntry);
}
    :   data_description_entry
    |   rename_description_entry
    |   condition_description_entry
    ;

/*------------------------------------------------------------------
 * Regular data item entries such as 01 A PIC X.
 *------------------------------------------------------------------*/
data_description_entry
    :   ^(DATA_ITEM data_item_level data_item_name? clauses* (data_entry[$data_entry::dataEntry.getChildren()])*)
    ;
    
data_item_level
    :   ^(LEVEL DATA_ITEM_LEVEL)
        {$data_entry::dataEntry.setLevelNumber(Integer.parseInt($DATA_ITEM_LEVEL.text));}
    ;

data_item_name
    :   ^(NAME DATA_NAME)
        {$data_entry::dataEntry.setCobolName($DATA_NAME.text);}
    ;
  
/*------------------------------------------------------------------
 * A rename expression such as: 66 NEWN RENAMES OLD.
 *------------------------------------------------------------------*/
rename_description_entry
    :   ^(RENAME rename_level data_item_name (rename_subject_literal | rename_subject_range))
    ; 
    
rename_level
    :   ^(LEVEL RENAMES_LEVEL)
        {$data_entry::dataEntry.setLevelNumber(Integer.parseInt($RENAMES_LEVEL.text));}
    ;

rename_subject_literal
    :   ^(LITERAL DATA_NAME)
        {$data_entry::dataEntry.setRenamesSubject($DATA_NAME.text);}
    ;

rename_subject_range
    :   ^(RANGE v=DATA_NAME w=DATA_NAME)
        {$data_entry::dataEntry.setRenamesSubjectRange(new Range($v.text, $w.text));}
    ;

/*------------------------------------------------------------------
 * A condition such as: 88 TRUE VALUE 1.
 *------------------------------------------------------------------*/
condition_description_entry
    :   ^(CONDITION condition_level data_item_name (condition_subject_literal | condition_subject_range)+)
    ; 

condition_level
    :   ^(LEVEL CONDITION_LEVEL)
        {$data_entry::dataEntry.setLevelNumber(Integer.parseInt($CONDITION_LEVEL.text));}
    ;

condition_subject_literal
    :   ^(LITERAL literal)
        {$data_entry::dataEntry.addConditionLiterals($literal.value);}
    ;

condition_subject_range
    :   ^(RANGE v=literal w=literal)
        {$data_entry::dataEntry.addConditionRange(new Range($v.value, $w.value));}
    ;

/*------------------------------------------------------------------
 * Regular data description entry clauses.
 *------------------------------------------------------------------*/
clauses 
    :   redefines_clause
    |   blank_when_zero_clause
    |   external_clause
    |   global_clause
    |   group_usage_clause
    |   justified_clause
    |   occurs_clause
    |   picture_clause
    |   sign_clause
    |   synchronized_clause
    |   usage_clause
    |   value_clause
    |   date_format_clause
    ; 

redefines_clause
    :   ^(REDEFINES DATA_NAME)
        {$data_entry::dataEntry.setRedefines($DATA_NAME.text);}
    ;

blank_when_zero_clause
    :   BLANKWHENZERO
        {$data_entry::dataEntry.setBlankWhenZero(true);}
    ;

external_clause
    :   EXTERNAL
        {$data_entry::dataEntry.setExternal(true);}
    ;

global_clause
    :   GLOBAL
        {$data_entry::dataEntry.setGlobal(true);}
    ;

group_usage_clause
    :   GROUPUSAGENATIONAL
        {$data_entry::dataEntry.setGroupUsageNational(true);}
    ;

justified_clause
    :   JUSTIFIEDRIGHT
        {$data_entry::dataEntry.setJustifiedRight(true);}
    ;

occurs_clause
    :   fixed_length_table
    |   variable_length_table
    ;

picture_clause
    :   ^(PICTURE PICTURESTRING)
        {$data_entry::dataEntry.setPicture($PICTURESTRING.text);}
    ;

sign_clause
    :   ^(SIGN sign_leading_clause? sign_trailing_clause?)
        {$data_entry::dataEntry.setSign(true);}
    ;
    
sign_leading_clause
    :   ^(LEADING separate_clause?)
        {$data_entry::dataEntry.setSignLeading(true);}
    ;
    
sign_trailing_clause
    :   ^(TRAILING separate_clause?)
        {$data_entry::dataEntry.setSignLeading(false);}
    ;
    
separate_clause
    :   SEPARATE
        {$data_entry::dataEntry.setSignSeparate(true);}
    ;

synchronized_clause
    :   ^(SYNCHRONIZED (LEFT | RIGHT)?)
        {$data_entry::dataEntry.setSynchronized(true);}
    ;

usage_clause
    :   ^(USAGE BINARY)
        {$data_entry::dataEntry.setUsage(Usage.BINARY);}
    |   ^(USAGE SINGLEFLOAT)
        {$data_entry::dataEntry.setUsage(Usage.SINGLEFLOAT);}
    |   ^(USAGE DOUBLEFLOAT)
        {$data_entry::dataEntry.setUsage(Usage.DOUBLEFLOAT);}
    |   ^(USAGE PACKEDDECIMAL)
        {$data_entry::dataEntry.setUsage(Usage.PACKEDDECIMAL);}
    |   ^(USAGE NATIVEBINARY)
        {$data_entry::dataEntry.setUsage(Usage.NATIVEBINARY);}
    |   ^(USAGE DISPLAY)
        {$data_entry::dataEntry.setUsage(Usage.DISPLAY);}
    |   ^(USAGE DISPLAY1)
        {$data_entry::dataEntry.setUsage(Usage.DISPLAY1);}
    |   ^(USAGE INDEX)
        {$data_entry::dataEntry.setUsage(Usage.INDEX);}
    |   ^(USAGE NATIONAL)
        {$data_entry::dataEntry.setUsage(Usage.NATIONAL);}
    |   ^(USAGE POINTER)
        {$data_entry::dataEntry.setUsage(Usage.POINTER);}
    |   ^(USAGE PROCEDUREPOINTER)
        {$data_entry::dataEntry.setUsage(Usage.PROCEDUREPOINTER);}
    |   ^(USAGE FUNCTIONPOINTER)
        {$data_entry::dataEntry.setUsage(Usage.FUNCTIONPOINTER);}
    ;

value_clause
    :   ^(VALUE value_clause_literal)
    ;
    
value_clause_literal
    :   literal
        {$data_entry::dataEntry.setValue($literal.value);}
    ;
    
literal returns [String value]
    :   FLOAT_LITERAL
        {$value = $FLOAT_LITERAL.text;}
    |   DECIMAL_LITERAL
        {$value = $DECIMAL_LITERAL.text;}
    |   INT
        {$value = $INT.text;}
    |   SIGNED_INT
        {$value = $SIGNED_INT.text;}
    |   ALPHANUM_LITERAL_STRING
        {$value = $ALPHANUM_LITERAL_STRING.text;}
    |   HEX_LITERAL_STRING
        {$value = $HEX_LITERAL_STRING.text;}
    |   ZERO_LITERAL_STRING
        {$value = $ZERO_LITERAL_STRING.text;}
    |   DBCS_LITERAL_STRING
        {$value = $DBCS_LITERAL_STRING.text;}
    |   NATIONAL_LITERAL_STRING
        {$value = $NATIONAL_LITERAL_STRING.text;}
    |   NATIONAL_HEX_LITERAL_STRING
        {$value = $NATIONAL_HEX_LITERAL_STRING.text;}
    |   ZERO_CONSTANT
        {$value = $ZERO_CONSTANT.text;}
    |   SPACE_CONSTANT
        {$value = $SPACE_CONSTANT.text;}
    |   HIGH_VALUE_CONSTANT
        {$value = $HIGH_VALUE_CONSTANT.text;}
    |   LOW_VALUE_CONSTANT
        {$value = $LOW_VALUE_CONSTANT.text;}
    |   QUOTE_CONSTANT
        {$value = $QUOTE_CONSTANT.text;}
    |   ALL_CONSTANT (v=ALPHANUM_LITERAL_STRING | v=ZERO_CONSTANT | v=SPACE_CONSTANT | v=HIGH_VALUE_CONSTANT | v=LOW_VALUE_CONSTANT | v=QUOTE_CONSTANT | v=NULL_CONSTANT)
        {$value = $ALL_CONSTANT.text + ' ' + $v.text;}
    |   NULL_CONSTANT
        {$value = $NULL_CONSTANT.text;}
    ;

date_format_clause
    :   ^(DATEFORMAT DATE_PATTERN)
        {$data_entry::dataEntry.setDateFormat($DATE_PATTERN.text);}
    ;
  
/*------------------------------------------------------------------
 * Arrays
 *------------------------------------------------------------------*/
fixed_length_table
    :   ^(FIXEDARRAY high_bound key_clause* index_clause*)
    ;               

variable_length_table
    :   ^(VARARRAY low_bound? high_bound key_clause* index_clause*)
    ;
    
high_bound
    :   ^(HBOUND INT depending_on?)
        {$data_entry::dataEntry.setMaxOccurs(Integer.parseInt($INT.text));}
    ;
    
depending_on
    :   ^(DEPENDINGON DATA_NAME) 
        {$data_entry::dataEntry.setDependingOn($DATA_NAME.text);}
    ; 

low_bound
    :   ^(LBOUND INT)
        {$data_entry::dataEntry.setMinOccurs(Integer.parseInt($INT.text));}
    ;         

key_clause
    :   ^(KEY ASCENDING_KEYWORD DATA_NAME)
        {$data_entry::dataEntry.addAscendingKey($DATA_NAME.text);}
    |   ^(KEY DESCENDING_KEYWORD DATA_NAME)
        {$data_entry::dataEntry.addDescendingKey($DATA_NAME.text);}
    ;
  
index_clause
    :   ^(INDEX DATA_NAME)
        {$data_entry::dataEntry.addIndex($DATA_NAME.text);}
    ; 
  
