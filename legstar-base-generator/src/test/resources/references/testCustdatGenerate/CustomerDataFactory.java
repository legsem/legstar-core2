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

        CobolStringType customerName =
                new CobolStringType.Builder()
                        .charNum(20)
                        .build();
        fields.put("customerName", customerName);

        CobolStringType customerAddress =
                new CobolStringType.Builder()
                        .charNum(20)
                        .build();
        fields.put("customerAddress", customerAddress);

        CobolStringType customerPhone =
                new CobolStringType.Builder()
                        .charNum(8)
                        .build();
        fields.put("customerPhone", customerPhone);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createFiller12() {

        final String complexTypeName = "Filler12";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType transactionDay =
                new CobolStringType.Builder()
                        .charNum(2)
                        .build();
        fields.put("transactionDay", transactionDay);

        CobolStringType filler14 =
                new CobolStringType.Builder()
                        .charNum(1)
                        .build();
        fields.put("filler14", filler14);

        CobolStringType transactionMonth =
                new CobolStringType.Builder()
                        .charNum(2)
                        .build();
        fields.put("transactionMonth", transactionMonth);

        CobolStringType filler16 =
                new CobolStringType.Builder()
                        .charNum(1)
                        .build();
        fields.put("filler16", filler16);

        CobolStringType transactionYear =
                new CobolStringType.Builder()
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

        CobolStringType transactionComment =
                new CobolStringType.Builder()
                        .charNum(9)
                        .build();
        fields.put("transactionComment", transactionComment);

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createTransactions() {

        final String complexTypeName = "Transactions";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolBinaryType < java.lang.Long > transactionNbr =
                new CobolBinaryType.Builder < java.lang.Long >(java.lang.Long.class)
                        .totalDigits(9)
                        .minInclusive(java.lang.Long.valueOf("0"))
                        .maxInclusive(java.lang.Long.valueOf("5"))
                        .odoObject(true)
                        .build();
        fields.put("transactionNbr", transactionNbr);

        fields.put("transaction", new CobolArrayType(createTransaction(), 5, "transactionNbr"));

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolComplexType createCustomerData() {

        final String complexTypeName = "CustomerData";
        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolZonedDecimalType < java.lang.Long > customerId =
                new CobolZonedDecimalType.Builder < java.lang.Long >(java.lang.Long.class)
                        .totalDigits(6)
                        .build();
        fields.put("customerId", customerId);

        fields.put("personalData", createPersonalData());

        fields.put("transactions", createTransactions());

        return new CobolComplexType(complexTypeName, fields);

    }

    public static CobolChoiceType createTransactionDateChoice() {

        Map < String, CobolType > fields = new LinkedHashMap < String, CobolType >();

        CobolStringType transactionDate =
                new CobolStringType.Builder()
                        .charNum(8)
                        .build();
        fields.put("transactionDate", transactionDate);

        fields.put("filler12", createFiller12());

        return new CobolChoiceType(fields);

    }

}
