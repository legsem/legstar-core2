package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CobolCustomerData extends CobolComplexType {

    public CobolCustomerData() {
        super("CustomerData", createCustomerDataFields());
    }

    private static Map < String, CobolType > createPersonalDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > customerName =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(20)
                        .build();
        fields.put("customerName", customerName);

        CobolStringType < String > customerAddress =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(20)
                        .build();
        fields.put("customerAddress", customerAddress);

        CobolStringType < String > customerPhone =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("customerPhone", customerPhone);

        return fields;

    }

    private static Map < String, CobolType > createFiller12Fields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > transactionDay =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(2)
                        .build();
        fields.put("transactionDay", transactionDay);

        CobolStringType < String > filler14 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("filler14", filler14);

        CobolStringType < String > transactionMonth =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(2)
                        .build();
        fields.put("transactionMonth", transactionMonth);

        CobolStringType < String > filler16 =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(1)
                        .build();
        fields.put("filler16", filler16);

        CobolStringType < String > transactionYear =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(2)
                        .build();
        fields.put("transactionYear", transactionYear);

        return fields;

    }

    private static Map < String, CobolType > createTransactionFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("transactionDateChoice", new CobolChoiceType("TransactionDateChoice",  createTransactionDateChoiceFields()));

        CobolPackedDecimalType < java.math.BigDecimal > transactionAmount =
                new CobolPackedDecimalType.Builder < java.math.BigDecimal >(java.math.BigDecimal.class)
                        .signed(true)
                        .totalDigits(15)
                        .fractionDigits(2)
                        .build();
        fields.put("transactionAmount", transactionAmount);

        CobolStringType < String > transactionComment =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(9)
                        .build();
        fields.put("transactionComment", transactionComment);

        return fields;

    }

    private static Map < String, CobolType > createTransactionsFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Long > transactionNbr =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("transactionNbr", transactionNbr);

        fields.put("transaction", new CobolArrayType(new CobolComplexType("Transaction",  createTransactionFields()), 5, "transactionNbr"));

        return fields;

    }

    private static Map < String, CobolType > createCustomerDataFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > customerId =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("customerId", customerId);

        fields.put("personalData", new CobolComplexType("PersonalData",  createPersonalDataFields()));

        fields.put("transactions", new CobolComplexType("Transactions",  createTransactionsFields()));

        return fields;

    }

    private static Map < String, CobolType > createTransactionDateChoiceFields() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > transactionDate =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("transactionDate", transactionDate);

        fields.put("filler12", new CobolComplexType("Filler12",  createFiller12Fields()));

        return fields;

    }

}
