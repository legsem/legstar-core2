package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Stru04RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComArray2".equals(type.getName())) {
            return new ComArray2Jaxb();
        }
        if ("ComGroup1".equals(type.getName())) {
            return new ComGroup1Jaxb();
        }
        if ("ComArray1".equals(type.getName())) {
            return new ComArray1Jaxb();
        }
        if ("Stru04Record".equals(type.getName())) {
            return new Stru04RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComArray2".equals(type.getName())) {
            return new ComArray2Jaxb((legstar.test.jaxb.stru04.ComArray2) jaxb);
        }
        if ("ComGroup1".equals(type.getName())) {
            return new ComGroup1Jaxb((legstar.test.jaxb.stru04.ComGroup1) jaxb);
        }
        if ("ComArray1".equals(type.getName())) {
            return new ComArray1Jaxb((legstar.test.jaxb.stru04.ComArray1) jaxb);
        }
        if ("Stru04Record".equals(type.getName())) {
            return new Stru04RecordJaxb((legstar.test.jaxb.stru04.Stru04Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComArray2Jaxb extends JaxbWrapper<legstar.test.jaxb.stru04.ComArray2> {

        public ComArray2Jaxb() {
            this(new legstar.test.jaxb.stru04.ComArray2());
        }

        public ComArray2Jaxb(legstar.test.jaxb.stru04.ComArray2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComItem4((String) value);
                break;
            case 1:
                getJaxb().getComArray3().clear();
                if (value instanceof java.util.List) {
                    for (Object item : (java.util.List<?>) value) {
                        if (item instanceof String){
                            getJaxb().getComArray3().add((String) item);
                        }
                    }
                }
                break;
            case 2:
                getJaxb().setComItem5((java.math.BigDecimal) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray2", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem4();
            case 1:
                return getJaxb().getComArray3();
            case 2:
                return getJaxb().getComItem5();
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray2", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comItem4=");
            builder.append(getJaxb().getComItem4());
            builder.append(", ");
            builder.append("comArray3=");
            builder.append(getJaxb().getComArray3());
            builder.append(", ");
            builder.append("comItem5=");
            builder.append(getJaxb().getComItem5());
            builder.append("}");
            return builder.toString();
        }

    }
    public class ComGroup1Jaxb extends JaxbWrapper<legstar.test.jaxb.stru04.ComGroup1> {

        public ComGroup1Jaxb() {
            this(new legstar.test.jaxb.stru04.ComGroup1());
        }

        public ComGroup1Jaxb(legstar.test.jaxb.stru04.ComGroup1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComItem3((Short) value);
                break;
            case 1:
                getJaxb().getComArray2().clear();
                if (value instanceof java.util.List) {
                    for (Object wrapperItem : (java.util.List<?>) value) {
                        if (wrapperItem instanceof ComArray2Jaxb){
                            getJaxb().getComArray2().add(((ComArray2Jaxb) wrapperItem).getJaxb());
                        }
                    }
                }
                break;
            case 2:
                getJaxb().setComItem6((Short) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComGroup1", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem3();
            case 1:
                java.util.List < ComArray2Jaxb > comArray2 = new java.util.ArrayList < ComArray2Jaxb >();
                for (legstar.test.jaxb.stru04.ComArray2 jaxbItem : getJaxb().getComArray2()) {
                    comArray2.add(new ComArray2Jaxb(jaxbItem));
                }
                return comArray2;
            case 2:
                return getJaxb().getComItem6();
            default:
                throw new InvalidComplexTypeFieldIndex("ComGroup1", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comItem3=");
            builder.append(getJaxb().getComItem3());
            builder.append(", ");
            builder.append("comArray2=");
            java.util.List < ComArray2Jaxb > comArray2 = new java.util.ArrayList < ComArray2Jaxb >();
            for (legstar.test.jaxb.stru04.ComArray2 jaxbItem : getJaxb().getComArray2()) {
                comArray2.add(new ComArray2Jaxb(jaxbItem));
            }
            builder.append(comArray2);
            builder.append(", ");
            builder.append("comItem6=");
            builder.append(getJaxb().getComItem6());
            builder.append("}");
            return builder.toString();
        }

    }
    public class ComArray1Jaxb extends JaxbWrapper<legstar.test.jaxb.stru04.ComArray1> {

        public ComArray1Jaxb() {
            this(new legstar.test.jaxb.stru04.ComArray1());
        }

        public ComArray1Jaxb(legstar.test.jaxb.stru04.ComArray1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComItem2((Short) value);
                break;
            case 1:
                getJaxb().setComGroup1(((ComGroup1Jaxb) value).getJaxb());
                break;
            case 2:
                getJaxb().setComItem7((Integer) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray1", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem2();
            case 1:
                return new ComGroup1Jaxb(getJaxb().getComGroup1());
            case 2:
                return getJaxb().getComItem7();
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray1", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comItem2=");
            builder.append(getJaxb().getComItem2());
            builder.append(", ");
            builder.append("comGroup1=");
            builder.append(new ComGroup1Jaxb(getJaxb().getComGroup1()));
            builder.append(", ");
            builder.append("comItem7=");
            builder.append(getJaxb().getComItem7());
            builder.append("}");
            return builder.toString();
        }

    }
    public class Stru04RecordJaxb extends JaxbWrapper<legstar.test.jaxb.stru04.Stru04Record> {

        public Stru04RecordJaxb() {
            this(new legstar.test.jaxb.stru04.Stru04Record());
        }

        public Stru04RecordJaxb(legstar.test.jaxb.stru04.Stru04Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComItem1((java.math.BigDecimal) value);
                break;
            case 1:
                getJaxb().getComArray1().clear();
                if (value instanceof java.util.List) {
                    for (Object wrapperItem : (java.util.List<?>) value) {
                        if (wrapperItem instanceof ComArray1Jaxb){
                            getJaxb().getComArray1().add(((ComArray1Jaxb) wrapperItem).getJaxb());
                        }
                    }
                }
                break;
            case 2:
                getJaxb().setComItem8((java.math.BigDecimal) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Stru04Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem1();
            case 1:
                java.util.List < ComArray1Jaxb > comArray1 = new java.util.ArrayList < ComArray1Jaxb >();
                for (legstar.test.jaxb.stru04.ComArray1 jaxbItem : getJaxb().getComArray1()) {
                    comArray1.add(new ComArray1Jaxb(jaxbItem));
                }
                return comArray1;
            case 2:
                return getJaxb().getComItem8();
            default:
                throw new InvalidComplexTypeFieldIndex("Stru04Record", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comItem1=");
            builder.append(getJaxb().getComItem1());
            builder.append(", ");
            builder.append("comArray1=");
            java.util.List < ComArray1Jaxb > comArray1 = new java.util.ArrayList < ComArray1Jaxb >();
            for (legstar.test.jaxb.stru04.ComArray1 jaxbItem : getJaxb().getComArray1()) {
                comArray1.add(new ComArray1Jaxb(jaxbItem));
            }
            builder.append(comArray1);
            builder.append(", ");
            builder.append("comItem8=");
            builder.append(getJaxb().getComItem8());
            builder.append("}");
            return builder.toString();
        }

    }

}
