package com.legstar.base.type.gen;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolCustomerData extends CobolComplexType {

    public CobolCustomerData() {
        super(new CobolComplexType.Builder()
                    .name("CustomerData")
                    .cobolName("CUSTOMER-DATA")
                    .fields(createCustomerDataFields())
              );
    }

    private static Map < String, CobolType > createPersonalDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > customerName =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CUSTOMER-NAME")
                        .charNum(20)
                        .build();
        fields.put("customerName", customerName);

        CobolStringType < String > customerAddress =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CUSTOMER-ADDRESS")
                        .charNum(20)
                        .build();
        fields.put("customerAddress", customerAddress);

        CobolStringType < String > customerPhone =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("CUSTOMER-PHONE")
                        .charNum(8)
                        .build();
        fields.put("customerPhone", customerPhone);

        return fields;

    }

    private static Map < String, CobolType > createFiller12Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > transactionDay =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("TRANSACTION-DAY")
                        .charNum(2)
                        .build();
        fields.put("transactionDay", transactionDay);

        CobolStringType < String > filler14 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(1)
                        .build();
        fields.put("filler14", filler14);

        CobolStringType < String > transactionMonth =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("TRANSACTION-MONTH")
                        .charNum(2)
                        .build();
        fields.put("transactionMonth", transactionMonth);

        CobolStringType < String > filler16 =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("FILLER")
                        .charNum(1)
                        .build();
        fields.put("filler16", filler16);

        CobolStringType < String > transactionYear =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("TRANSACTION-YEAR")
                        .charNum(2)
                        .build();
        fields.put("transactionYear", transactionYear);

        return fields;

    }

    private static Map < String, CobolType > createTransactionFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolChoiceType transactionDateChoice = new CobolChoiceType.Builder()
                        .name("TransactionDateChoice")
                        .alternatives(createTransactionDateChoiceFields())
                        .build();
        fields.put("transactionDateChoice", transactionDateChoice);

        CobolPackedDecimalType < java.math.BigDecimal > transactionAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .cobolName("TRANSACTION-AMOUNT")
                        .signed(true)
                        .totalDigits(15)
                        .fractionDigits(2)
                        .build();
        fields.put("transactionAmount", transactionAmount);

        CobolStringType < String > transactionComment =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("TRANSACTION-COMMENT")
                        .charNum(9)
                        .build();
        fields.put("transactionComment", transactionComment);

        return fields;

    }

    private static Map < String, CobolType > createTransactionsFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Long > transactionNbr =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .cobolName("TRANSACTION-NBR")
                        .totalDigits(9)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("transactionNbr", transactionNbr);

        CobolComplexType transaction = createTransaction();
        CobolArrayType transactionArray = new CobolArrayType.Builder()
                        .itemType(transaction)
                        .minOccurs(0)
                        .maxOccurs(5)
                        .dependingOn("transactionNbr")
                        .build();
        fields.put("transaction", transactionArray);

        return fields;

    }

    private static Map < String, CobolType > createCustomerDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > customerId =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .cobolName("CUSTOMER-ID")
                        .totalDigits(6)
                        .build();
        fields.put("customerId", customerId);

        CobolComplexType personalData = createPersonalData();
        fields.put("personalData", personalData);

        CobolComplexType transactions = createTransactions();
        fields.put("transactions", transactions);

        return fields;

    }

    private static Map < String, CobolType > createTransactionDateChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > transactionDate =
                new CobolStringType.Builder < String >(String.class)
                        .cobolName("TRANSACTION-DATE")
                        .charNum(8)
                        .build();
        fields.put("transactionDate", transactionDate);

        CobolComplexType filler12 = createFiller12();
        fields.put("filler12", filler12);

        return fields;

    }
    public static CobolComplexType createTransaction() {

        return new CobolComplexType.Builder()
                .name("Transaction")
                .cobolName("TRANSACTION")
                .fields(createTransactionFields())
                .build();
    }

    public static CobolComplexType createPersonalData() {

        return new CobolComplexType.Builder()
                .name("PersonalData")
                .cobolName("PERSONAL-DATA")
                .fields(createPersonalDataFields())
                .build();
    }

    public static CobolComplexType createTransactions() {

        return new CobolComplexType.Builder()
                .name("Transactions")
                .cobolName("TRANSACTIONS")
                .fields(createTransactionsFields())
                .build();
    }

    public static CobolComplexType createFiller12() {

        return new CobolComplexType.Builder()
                .name("Filler12")
                .cobolName("FILLER")
                .fields(createFiller12Fields())
                .build();
    }


}

