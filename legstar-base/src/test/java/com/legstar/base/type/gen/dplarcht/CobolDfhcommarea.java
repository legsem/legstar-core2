package com.legstar.base.type.gen.dplarcht;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolDfhcommarea extends CobolComplexType {

    public CobolDfhcommarea() {
        super(new CobolComplexType.Builder()
                    .name("Dfhcommarea")
                    .cobolName("DFHCOMMAREA")
                    .fields(createDfhcommareaFields())
              );
    }

    private static Map < String, CobolType > createLsSearchCriteriaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsStartwith =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-STARTWITH")
                        .charNum(8)
                        .build();
        fields.put("lsStartwith", lsStartwith);

        CobolPackedDecimalType < Long > lsStartwithLen =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .cobolName("LS-STARTWITH-LEN")
                        .totalDigits(9)
                        .build();
        fields.put("lsStartwithLen", lsStartwithLen);

        return fields;

    }

    private static Map < String, CobolType > createLsRequestFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > lsRequestType =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("LS-REQUEST-TYPE")
                        .totalDigits(4)
                        .build();
        fields.put("lsRequestType", lsRequestType);

        CobolChoiceType lsAllItemsChoice = new CobolChoiceType.Builder()
                        .name("LsAllItemsChoice")
                        .alternatives(createLsAllItemsChoiceFields())
                        .build();
        fields.put("lsAllItemsChoice", lsAllItemsChoice);

        CobolComplexType lsSearchCriteria = createLsSearchCriteria();
        fields.put("lsSearchCriteria", lsSearchCriteria);

        return fields;

    }

    private static Map < String, CobolType > createLsFilesDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsFileName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-FILE-NAME")
                        .charNum(8)
                        .build();
        fields.put("lsFileName", lsFileName);

        CobolStringType < String > lsFileDsname =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-FILE-DSNAME")
                        .charNum(44)
                        .build();
        fields.put("lsFileDsname", lsFileDsname);

        CobolStringType < String > lsFileEnablestatus =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-FILE-ENABLESTATUS")
                        .charNum(12)
                        .build();
        fields.put("lsFileEnablestatus", lsFileEnablestatus);

        return fields;

    }

    private static Map < String, CobolType > createLsProgramsDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsProgramName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-PROGRAM-NAME")
                        .charNum(8)
                        .build();
        fields.put("lsProgramName", lsProgramName);

        CobolStringType < String > lsProgramType =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-PROGRAM-TYPE")
                        .charNum(12)
                        .build();
        fields.put("lsProgramType", lsProgramType);

        CobolStringType < String > lsProgramLanguage =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-PROGRAM-LANGUAGE")
                        .charNum(12)
                        .build();
        fields.put("lsProgramLanguage", lsProgramLanguage);

        CobolBinaryType < Integer > lsProgramLength =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("LS-PROGRAM-LENGTH")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("lsProgramLength", lsProgramLength);

        CobolBinaryType < Integer > lsProgramUsecount =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("LS-PROGRAM-USECOUNT")
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("lsProgramUsecount", lsProgramUsecount);

        CobolStringType < String > filler113 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(24)
                        .build();
        fields.put("filler113", filler113);

        return fields;

    }

    private static Map < String, CobolType > createLsTransactionsDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsTransactionName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-TRANSACTION-NAME")
                        .charNum(8)
                        .build();
        fields.put("lsTransactionName", lsTransactionName);

        CobolStringType < String > lsTransactionProgram =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-TRANSACTION-PROGRAM")
                        .charNum(8)
                        .build();
        fields.put("lsTransactionProgram", lsTransactionProgram);

        CobolStringType < String > lsTransactionStatus =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-TRANSACTION-STATUS")
                        .charNum(12)
                        .build();
        fields.put("lsTransactionStatus", lsTransactionStatus);

        CobolStringType < String > filler119 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(36)
                        .build();
        fields.put("filler119", filler119);

        return fields;

    }

    private static Map < String, CobolType > createLsItemsArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolChoiceType lsFilesDataChoice = new CobolChoiceType.Builder()
                        .name("LsFilesDataChoice")
                        .alternatives(createLsFilesDataChoiceFields())
                        .build();
        fields.put("lsFilesDataChoice", lsFilesDataChoice);

        return fields;

    }

    private static Map < String, CobolType > createLsReplyDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Long > lsItemsCount =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("LS-ITEMS-COUNT")
                        .totalDigits(9)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("500"))
                        .odoObject(true)
                        .build();
        fields.put("lsItemsCount", lsItemsCount);

        CobolComplexType lsItemsArray = createLsItemsArray();
        CobolArrayType lsItemsArrayArray = new CobolArrayType.Builder()
                        .itemType(lsItemsArray)
                        .minOccurs(0)
                        .maxOccurs(500)
                        .dependingOn("lsItemsCount")
                        .build();
        fields.put("lsItemsArray", lsItemsArrayArray);

        return fields;

    }

    private static Map < String, CobolType > createLsReplyFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > lsReplyType =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .cobolName("LS-REPLY-TYPE")
                        .totalDigits(4)
                        .build();
        fields.put("lsReplyType", lsReplyType);

        CobolComplexType lsReplyData = createLsReplyData();
        fields.put("lsReplyData", lsReplyData);

        return fields;

    }

    private static Map < String, CobolType > createDfhcommareaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType lsRequest = createLsRequest();
        fields.put("lsRequest", lsRequest);

        CobolComplexType lsReply = createLsReply();
        fields.put("lsReply", lsReply);

        return fields;

    }

    private static Map < String, CobolType > createLsAllItemsChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsAllItems =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("LS-ALL-ITEMS")
                        .charNum(4)
                        .build();
        fields.put("lsAllItems", lsAllItems);

        CobolZonedDecimalType < Integer > lsMaxItems =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .cobolName("LS-MAX-ITEMS")
                        .totalDigits(4)
                        .build();
        fields.put("lsMaxItems", lsMaxItems);

        return fields;

    }
    private static Map < String, CobolType > createLsFilesDataChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolComplexType lsFilesData = createLsFilesData();
        fields.put("lsFilesData", lsFilesData);

        CobolComplexType lsProgramsData = createLsProgramsData();
        fields.put("lsProgramsData", lsProgramsData);

        CobolComplexType lsTransactionsData = createLsTransactionsData();
        fields.put("lsTransactionsData", lsTransactionsData);

        return fields;

    }
    public static CobolComplexType createLsSearchCriteria() {

        return new CobolComplexType.Builder()
                .name("LsSearchCriteria")
                .cobolName("LS-SEARCH-CRITERIA")
                .fields(createLsSearchCriteriaFields())
                .build();
    }

    public static CobolComplexType createLsItemsArray() {

        return new CobolComplexType.Builder()
                .name("LsItemsArray")
                .cobolName("LS-ITEMS-ARRAY")
                .fields(createLsItemsArrayFields())
                .build();
    }

    public static CobolComplexType createLsReplyData() {

        return new CobolComplexType.Builder()
                .name("LsReplyData")
                .cobolName("LS-REPLY-DATA")
                .fields(createLsReplyDataFields())
                .build();
    }

    public static CobolComplexType createLsRequest() {

        return new CobolComplexType.Builder()
                .name("LsRequest")
                .cobolName("LS-REQUEST")
                .fields(createLsRequestFields())
                .build();
    }

    public static CobolComplexType createLsReply() {

        return new CobolComplexType.Builder()
                .name("LsReply")
                .cobolName("LS-REPLY")
                .fields(createLsReplyFields())
                .build();
    }

    public static CobolComplexType createLsFilesData() {

        return new CobolComplexType.Builder()
                .name("LsFilesData")
                .cobolName("LS-FILES-DATA")
                .fields(createLsFilesDataFields())
                .build();
    }

    public static CobolComplexType createLsProgramsData() {

        return new CobolComplexType.Builder()
                .name("LsProgramsData")
                .cobolName("LS-PROGRAMS-DATA")
                .fields(createLsProgramsDataFields())
                .build();
    }

    public static CobolComplexType createLsTransactionsData() {

        return new CobolComplexType.Builder()
                .name("LsTransactionsData")
                .cobolName("LS-TRANSACTIONS-DATA")
                .fields(createLsTransactionsDataFields())
                .build();
    }


}

