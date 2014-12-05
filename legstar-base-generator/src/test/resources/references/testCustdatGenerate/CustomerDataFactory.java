package test.example;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.base.type.CobolType;
import com.legstar.base.type.composite.*;
import com.legstar.base.type.primitive.*;

public class CustomerDataFactory {

    public static CobolComplexType create() {
        return createCustomerData();
    }

    public static CobolComplexType createPersonalData() {

        final String complexTypeName = "PersonalData";
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

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createFiller12() {

        final String complexTypeName = "Filler12";
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

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createTransaction() {

        final String complexTypeName = "Transaction";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        fields.put("transactionDateChoice", createTransactionDateChoice());

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

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createTransactions() {

        final String complexTypeName = "Transactions";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < Long > transactionNbr =
                new CobolBinaryType.Builder < Long >(Long.class)
                        .totalDigits(9)
                        .minInclusive(Long.valueOf("0"))
                        .maxInclusive(Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("transactionNbr", transactionNbr);

        fields.put("transaction", new CobolArrayType(createTransaction(), 5, "transactionNbr"));

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createCustomerData() {

        final String complexTypeName = "CustomerData";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < Long > customerId =
                new CobolZonedDecimalType.Builder < Long >(Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("customerId", customerId);

        fields.put("personalData", createPersonalData());

        fields.put("transactions", createTransactions());

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolChoiceType createTransactionDateChoice() {

        final String choiceTypeName = "TransactionDateChoice";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType < String > transactionDate =
                new CobolStringType.Builder < String >(String.class)
                        .charNum(8)
                        .build();
        fields.put("transactionDate", transactionDate);

        fields.put("filler12", createFiller12());

        return new CobolChoiceType(choiceTypeName, fields);

    }

}
