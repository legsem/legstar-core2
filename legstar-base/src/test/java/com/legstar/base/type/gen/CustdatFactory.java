package com.legstar.base.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.CobolArrayType;
import com.legstar.base.type.composite.CobolChoiceType;
import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.type.primitive.CobolBinaryType;
import com.legstar.base.type.primitive.CobolPackedDecimalType;
import com.legstar.base.type.primitive.CobolStringType;
import com.legstar.base.type.primitive.CobolZonedDecimalType;

public class CustdatFactory {

    public static CobolComplexType createCustomerDataCobolType() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("customerId",
                new CobolZonedDecimalType.Builder < Integer >(Integer.class)
                        .signed(false).signLeading(false).signSeparate(false)
                        .totalDigits(6).fractionDigits(0).build()); // Customer
                                                                    // ID
        children.put("personalData", createPersonalDataCobolType());
        children.put("transactions", createTransactionsCobolType());
        return new CobolComplexType("CustomerData", children);

    }

    public static CobolComplexType createPersonalDataCobolType() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("customerName", new CobolStringType.Builder <String>(String.class)
                .charNum(20)
                .build());
        children.put("customerAddress",
                new CobolStringType.Builder <String>(String.class)
                .charNum(20)
                .build());
        children.put("customerPhone", new CobolStringType.Builder <String>(String.class)
                .charNum(8)
                .build());
        return new CobolComplexType("PersonalData", children);
    }

    private static CobolComplexType createTransactionsCobolType() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put(
                "transactionNbr",
                new CobolBinaryType.Builder < Short >(Short.class)
                        .signed(false).totalDigits(9).fractionDigits(0)
                        .minInclusive(Short.valueOf("0"))
                        .maxInclusive(Short.valueOf("5")).odoObject(true)
                        .build());
        children.put("transaction", new CobolArrayType(
                createTransactionCobolType(), 5, "transactionNbr"));
        return new CobolComplexType("Transactions", children);
    }

    private static CobolComplexType createTransactionCobolType() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("transactionDateChoice", createTransactionDateChoice());
        children.put("transactionAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(
                        BigDecimal.class).signed(true).totalDigits(15)
                        .fractionDigits(2).build());
        children.put("transactionComment", new CobolStringType.Builder <String>(String.class)
                .charNum(9).build());
        return new CobolComplexType("Transaction", children);
    }

    public static CobolChoiceType createTransactionDateChoice() {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("transactionDate", new CobolStringType.Builder <String>(String.class)
                .charNum(8).build());
        alternatives.put("filler1", createFiller12CobolType());
        return new CobolChoiceType("TransactionDateChoice", alternatives);
    }

    private static CobolComplexType createFiller12CobolType() {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("transactionDay", new CobolStringType.Builder <String>(String.class)
                .charNum(2)
                .build());
        children.put("filler2", new CobolStringType.Builder <String>(String.class)
                .charNum(1)
                .build());
        children.put("transactionMonth",
                new CobolStringType.Builder <String>(String.class)
                .charNum(2).
                build());
        children.put("filler3", new CobolStringType.Builder <String>(String.class)
                .charNum(1)
                .build());
        children.put("transactionYear", new CobolStringType.Builder <String>(String.class)
                .charNum(2)
                .build());
        return new CobolComplexType("Filler12", children);
    }
}
