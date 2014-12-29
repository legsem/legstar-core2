package com.legstar.base.type.gen.dplarcht;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolDfhcommarea extends CobolComplexType {

    public CobolDfhcommarea() {
        super("Dfhcommarea", createDfhcommareaFields());
    }

    private static Map < String, CobolType > createLsSearchCriteriaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsStartwith =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("lsStartwith", lsStartwith);

        CobolPackedDecimalType < Long > lsStartwithLen =
                new CobolPackedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .build();
        fields.put("lsStartwithLen", lsStartwithLen);

        return fields;

    }

    private static Map < String, CobolType > createLsRequestFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > lsRequestType =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("lsRequestType", lsRequestType);

        fields.put("lsAllItemsChoice", new CobolChoiceType("LsAllItemsChoice",  createLsAllItemsChoiceFields()));

        fields.put("lsSearchCriteria", new CobolComplexType("LsSearchCriteria",  createLsSearchCriteriaFields()));

        return fields;

    }

    private static Map < String, CobolType > createLsFilesDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsFileName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("lsFileName", lsFileName);

        CobolStringType < String > lsFileDsname =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(44)
                        .build();
        fields.put("lsFileDsname", lsFileDsname);

        CobolStringType < String > lsFileEnablestatus =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(12)
                        .build();
        fields.put("lsFileEnablestatus", lsFileEnablestatus);

        return fields;

    }

    private static Map < String, CobolType > createLsProgramsDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsProgramName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("lsProgramName", lsProgramName);

        CobolStringType < String > lsProgramType =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(12)
                        .build();
        fields.put("lsProgramType", lsProgramType);

        CobolStringType < String > lsProgramLanguage =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(12)
                        .build();
        fields.put("lsProgramLanguage", lsProgramLanguage);

        CobolBinaryType < Integer > lsProgramLength =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("lsProgramLength", lsProgramLength);

        CobolBinaryType < Integer > lsProgramUsecount =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .signed(true)
                        .totalDigits(9)
                        .build();
        fields.put("lsProgramUsecount", lsProgramUsecount);

        CobolStringType < String > filler113 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(24)
                        .build();
        fields.put("filler113", filler113);

        return fields;

    }

    private static Map < String, CobolType > createLsTransactionsDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsTransactionName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("lsTransactionName", lsTransactionName);

        CobolStringType < String > lsTransactionProgram =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("lsTransactionProgram", lsTransactionProgram);

        CobolStringType < String > lsTransactionStatus =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(12)
                        .build();
        fields.put("lsTransactionStatus", lsTransactionStatus);

        CobolStringType < String > filler119 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(36)
                        .build();
        fields.put("filler119", filler119);

        return fields;

    }

    private static Map < String, CobolType > createLsItemsArrayFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("lsFilesDataChoice", new CobolChoiceType("LsFilesDataChoice",  createLsFilesDataChoiceFields()));

        return fields;

    }

    private static Map < String, CobolType > createLsReplyDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Long > lsItemsCount =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .minInclusive(Long.valueOf("1"))
                        .maxInclusive(Long.valueOf("500"))
                        .odoObject(true)
                        .build();
        fields.put("lsItemsCount", lsItemsCount);

        fields.put("lsItemsArray", new CobolArrayType(new CobolComplexType("LsItemsArray",  createLsItemsArrayFields()), 500, "lsItemsCount"));

        return fields;

    }

    private static Map < String, CobolType > createLsReplyFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Integer > lsReplyType =
                new CobolBinaryType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("lsReplyType", lsReplyType);

        fields.put("lsReplyData", new CobolComplexType("LsReplyData",  createLsReplyDataFields()));

        return fields;

    }

    private static Map < String, CobolType > createDfhcommareaFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("lsRequest", new CobolComplexType("LsRequest",  createLsRequestFields()));

        fields.put("lsReply", new CobolComplexType("LsReply",  createLsReplyFields()));

        return fields;

    }

    private static Map < String, CobolType > createLsAllItemsChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > lsAllItems =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(4)
                        .build();
        fields.put("lsAllItems", lsAllItems);

        CobolZonedDecimalType < Integer > lsMaxItems =
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .totalDigits(4)
                        .build();
        fields.put("lsMaxItems", lsMaxItems);

        return fields;

    }

    private static Map < String, CobolType > createLsFilesDataChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("lsFilesData", new CobolComplexType("LsFilesData",  createLsFilesDataFields()));

        fields.put("lsProgramsData", new CobolComplexType("LsProgramsData",  createLsProgramsDataFields()));

        fields.put("lsTransactionsData", new CobolComplexType("LsTransactionsData",  createLsTransactionsDataFields()));

        return fields;

    }

}
