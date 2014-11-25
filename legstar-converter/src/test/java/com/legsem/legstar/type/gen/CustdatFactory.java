package com.legsem.legstar.type.gen;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import com.legsem.legstar.context.CobolContext;
import com.legsem.legstar.type.CobolType;
import com.legsem.legstar.type.composite.CobolArrayType;
import com.legsem.legstar.type.composite.CobolChoiceType;
import com.legsem.legstar.type.composite.CobolComplexType;
import com.legsem.legstar.type.primitive.CobolBinaryType;
import com.legsem.legstar.type.primitive.CobolPackedDecimalType;
import com.legsem.legstar.type.primitive.CobolStringType;
import com.legsem.legstar.type.primitive.CobolZonedDecimalType;

public class CustdatFactory {

    public static CobolComplexType createCustomerDataCobolType(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("customerId",
                new CobolZonedDecimalType.Builder < Integer >(cobolContext,
                        Integer.class).signed(false).signLeading(false)
                        .signSeparate(false).totalDigits(6).fractionDigits(0)
                        .build()); // Customer ID
        children.put("personalData", createPersonalDataCobolType(cobolContext));
        children.put("transactions", createTransactionsCobolType(cobolContext));
        return new CobolComplexType(cobolContext, children);

    }

    public static CobolComplexType createPersonalDataCobolType(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("customerName", new CobolStringType.Builder(cobolContext)
                .charNum(20).build());
        children.put("customerAddress", new CobolStringType.Builder(
                cobolContext).charNum(20).build());
        children.put("customerPhone", new CobolStringType.Builder(cobolContext)
                .charNum(8).build());
        return new CobolComplexType(cobolContext, children);
    }

    private static CobolComplexType createTransactionsCobolType(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("transactionNbr", new CobolBinaryType.Builder < Short >(
                cobolContext, Short.class).signed(false).totalDigits(9)
                .fractionDigits(0).minInclusive(Short.valueOf("0"))
                .maxInclusive(Short.valueOf("5")).odoObject(true).build());
        children.put("transaction", new CobolArrayType(cobolContext,
                createTransactionCobolType(cobolContext), 5, "transactionNbr"));
        return new CobolComplexType(cobolContext, children);
    }

    private static CobolComplexType createTransactionCobolType(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("transactionDateChoice",
                createTransactionDateChoice(cobolContext));
        children.put("transactionAmount",
                new CobolPackedDecimalType.Builder < BigDecimal >(cobolContext,
                        BigDecimal.class).signed(true).totalDigits(15)
                        .fractionDigits(2).build());
        children.put("transactionComment", new CobolStringType.Builder(
                cobolContext).charNum(9).build());
        return new CobolComplexType(cobolContext, children);
    }

    public static CobolChoiceType createTransactionDateChoice(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > alternatives = new LinkedHashMap < String, CobolType >();
        alternatives.put("transactionDate", new CobolStringType.Builder(
                cobolContext).charNum(8).build());
        alternatives.put("filler1", createFiller12CobolType(cobolContext));
        return new CobolChoiceType(cobolContext, alternatives);
    }

    private static CobolComplexType createFiller12CobolType(
            CobolContext cobolContext) {
        LinkedHashMap < String, CobolType > children = new LinkedHashMap < String, CobolType >();
        children.put("transactionDay",
                new CobolStringType.Builder(cobolContext).charNum(2).build());
        children.put("filler2", new CobolStringType.Builder(cobolContext)
                .charNum(1).build());
        children.put("transactionMonth", new CobolStringType.Builder(
                cobolContext).charNum(2).build());
        children.put("filler3", new CobolStringType.Builder(cobolContext)
                .charNum(1).build());
        children.put("transactionYear", new CobolStringType.Builder(
                cobolContext).charNum(2).build());
        return new CobolComplexType(cobolContext, children);
    }
}
