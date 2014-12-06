package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceTypeAlternative;
import com.legstar.base.visitor.NoAlternativeForChoiceType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class CustomerDataJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("PersonalData".equals(type.getName())) {
            return new PersonalDataJaxb();
        }
        if ("Filler12".equals(type.getName())) {
            return new Filler12Jaxb();
        }
        if ("Transaction".equals(type.getName())) {
            return new TransactionJaxb();
        }
        if ("Transactions".equals(type.getName())) {
            return new TransactionsJaxb();
        }
        if ("CustomerData".equals(type.getName())) {
            return new CustomerDataJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("PersonalData".equals(type.getName())) {
            return new PersonalDataJaxb((legstar.test.jaxb.cusdat.PersonalData) jaxb);
        }
        if ("Filler12".equals(type.getName())) {
            return new Filler12Jaxb((legstar.test.jaxb.cusdat.Filler12) jaxb);
        }
        if ("Transaction".equals(type.getName())) {
            return new TransactionJaxb((legstar.test.jaxb.cusdat.Transaction) jaxb);
        }
        if ("Transactions".equals(type.getName())) {
            return new TransactionsJaxb((legstar.test.jaxb.cusdat.Transactions) jaxb);
        }
        if ("CustomerData".equals(type.getName())) {
            return new CustomerDataJaxb((legstar.test.jaxb.cusdat.CustomerData) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class PersonalDataJaxb extends JaxbWrapper<legstar.test.jaxb.cusdat.PersonalData> {

        public PersonalDataJaxb() {
            this(new legstar.test.jaxb.cusdat.PersonalData());
        }

        public PersonalDataJaxb(legstar.test.jaxb.cusdat.PersonalData jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCustomerName((String) value);
                break;
            case 1:
                getJaxb().setCustomerAddress((String) value);
                break;
            case 2:
                getJaxb().setCustomerPhone((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("PersonalData", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getCustomerName();
            case 1:
                return getJaxb().getCustomerAddress();
            case 2:
                return getJaxb().getCustomerPhone();
            default:
                throw new InvalidComplexTypeFieldIndex("PersonalData", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("customerName=");
            builder.append(getJaxb().getCustomerName());
            builder.append(", ");
            builder.append("customerAddress=");
            builder.append(getJaxb().getCustomerAddress());
            builder.append(", ");
            builder.append("customerPhone=");
            builder.append(getJaxb().getCustomerPhone());
            builder.append("}");
            return builder.toString();
        }

    }
    public class Filler12Jaxb extends JaxbWrapper<legstar.test.jaxb.cusdat.Filler12> {

        public Filler12Jaxb() {
            this(new legstar.test.jaxb.cusdat.Filler12());
        }

        public Filler12Jaxb(legstar.test.jaxb.cusdat.Filler12 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setTransactionDay((String) value);
                break;
            case 1:
                getJaxb().setFiller14((String) value);
                break;
            case 2:
                getJaxb().setTransactionMonth((String) value);
                break;
            case 3:
                getJaxb().setFiller16((String) value);
                break;
            case 4:
                getJaxb().setTransactionYear((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Filler12", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getTransactionDay();
            case 1:
                return getJaxb().getFiller14();
            case 2:
                return getJaxb().getTransactionMonth();
            case 3:
                return getJaxb().getFiller16();
            case 4:
                return getJaxb().getTransactionYear();
            default:
                throw new InvalidComplexTypeFieldIndex("Filler12", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("transactionDay=");
            builder.append(getJaxb().getTransactionDay());
            builder.append(", ");
            builder.append("filler14=");
            builder.append(getJaxb().getFiller14());
            builder.append(", ");
            builder.append("transactionMonth=");
            builder.append(getJaxb().getTransactionMonth());
            builder.append(", ");
            builder.append("filler16=");
            builder.append(getJaxb().getFiller16());
            builder.append(", ");
            builder.append("transactionYear=");
            builder.append(getJaxb().getTransactionYear());
            builder.append("}");
            return builder.toString();
        }

    }
    public class TransactionJaxb extends JaxbWrapper<legstar.test.jaxb.cusdat.Transaction> {

        public TransactionJaxb() {
            this(new legstar.test.jaxb.cusdat.Transaction());
        }

        public TransactionJaxb(legstar.test.jaxb.cusdat.Transaction jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                switch (alternativeIndex) {
                case 0:
                    getJaxb().setTransactionDate((String) value);
                    break;
                case 1:
                    getJaxb().setFiller12(((Filler12Jaxb) value).getJaxb());
                    break;
                default:
                    throw new InvalidChoiceTypeAlternative("TransactionDateChoice",
                            alternativeIndex);
                }
                break;
            case 1:
                getJaxb().setTransactionAmount((java.math.BigDecimal) value);
                break;
            case 2:
                getJaxb().setTransactionComment((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Transaction", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                if (getJaxb().getTransactionDate() != null) {
                    return getJaxb().getTransactionDate();
                } else if (getJaxb().getFiller12() != null) {
                    return new Filler12Jaxb(getJaxb().getFiller12());
                } else {
                    throw new NoAlternativeForChoiceType("TransactionDateChoice");
                }
            case 1:
                return getJaxb().getTransactionAmount();
            case 2:
                return getJaxb().getTransactionComment();
            default:
                throw new InvalidComplexTypeFieldIndex("Transaction", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("transactionDateChoice=");
            if (getJaxb().getTransactionDate() != null) {
                builder.append("transactionDate=");
                builder.append(getJaxb().getTransactionDate());
            }
            if (getJaxb().getFiller12() != null) {
                builder.append("filler12=");
                builder.append(new Filler12Jaxb(getJaxb().getFiller12()));
            }
            builder.append(", ");
            builder.append("transactionAmount=");
            builder.append(getJaxb().getTransactionAmount());
            builder.append(", ");
            builder.append("transactionComment=");
            builder.append(getJaxb().getTransactionComment());
            builder.append("}");
            return builder.toString();
        }

    }
    public class TransactionsJaxb extends JaxbWrapper<legstar.test.jaxb.cusdat.Transactions> {

        public TransactionsJaxb() {
            this(new legstar.test.jaxb.cusdat.Transactions());
        }

        public TransactionsJaxb(legstar.test.jaxb.cusdat.Transactions jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setTransactionNbr((Long) value);
                break;
            case 1:
                getJaxb().getTransaction().clear();
                if (value instanceof java.util.List) {
                    for (Object wrapperItem : (java.util.List<?>) value) {
                        if (wrapperItem instanceof TransactionJaxb){
                            getJaxb().getTransaction().add(((TransactionJaxb) wrapperItem).getJaxb());
                        }
                    }
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Transactions", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getTransactionNbr();
            case 1:
                java.util.List < TransactionJaxb > transaction = new java.util.ArrayList < TransactionJaxb >();
                for (legstar.test.jaxb.cusdat.Transaction jaxbItem : getJaxb().getTransaction()) {
                    transaction.add(new TransactionJaxb(jaxbItem));
                }
                return transaction;
            default:
                throw new InvalidComplexTypeFieldIndex("Transactions", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("transactionNbr=");
            builder.append(getJaxb().getTransactionNbr());
            builder.append(", ");
            builder.append("transaction=");
            java.util.List < TransactionJaxb > transaction = new java.util.ArrayList < TransactionJaxb >();
            for (legstar.test.jaxb.cusdat.Transaction jaxbItem : getJaxb().getTransaction()) {
                transaction.add(new TransactionJaxb(jaxbItem));
            }
            builder.append(transaction);
            builder.append("}");
            return builder.toString();
        }

    }
    public class CustomerDataJaxb extends JaxbWrapper<legstar.test.jaxb.cusdat.CustomerData> {

        public CustomerDataJaxb() {
            this(new legstar.test.jaxb.cusdat.CustomerData());
        }

        public CustomerDataJaxb(legstar.test.jaxb.cusdat.CustomerData jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCustomerId((Long) value);
                break;
            case 1:
                getJaxb().setPersonalData(((PersonalDataJaxb) value).getJaxb());
                break;
            case 2:
                getJaxb().setTransactions(((TransactionsJaxb) value).getJaxb());
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("CustomerData", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getCustomerId();
            case 1:
                return new PersonalDataJaxb(getJaxb().getPersonalData());
            case 2:
                return new TransactionsJaxb(getJaxb().getTransactions());
            default:
                throw new InvalidComplexTypeFieldIndex("CustomerData", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("customerId=");
            builder.append(getJaxb().getCustomerId());
            builder.append(", ");
            builder.append("personalData=");
            builder.append(new PersonalDataJaxb(getJaxb().getPersonalData()));
            builder.append(", ");
            builder.append("transactions=");
            builder.append(new TransactionsJaxb(getJaxb().getTransactions()));
            builder.append("}");
            return builder.toString();
        }

    }

}
