package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Stru03RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComArray".equals(type.getName())) {
            return new ComArrayJaxb();
        }
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComArray".equals(type.getName())) {
            return new ComArrayJaxb((legstar.test.jaxb.stru03.ComArray) jaxb);
        }
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordJaxb((legstar.test.jaxb.stru03.Stru03Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComArrayJaxb extends JaxbWrapper<legstar.test.jaxb.stru03.ComArray> {

        public ComArrayJaxb() {
            this(new legstar.test.jaxb.stru03.ComArray());
        }

        public ComArrayJaxb(legstar.test.jaxb.stru03.ComArray jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComItem1((Short) value);
                break;
            case 1:
                getJaxb().setComItem2((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem1();
            case 1:
                return getJaxb().getComItem2();
            default:
                throw new InvalidComplexTypeFieldIndex("ComArray", index);
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
            builder.append("comItem2=");
            builder.append(getJaxb().getComItem2());
            builder.append("}");
            return builder.toString();
        }

    }
    public class Stru03RecordJaxb extends JaxbWrapper<legstar.test.jaxb.stru03.Stru03Record> {

        public Stru03RecordJaxb() {
            this(new legstar.test.jaxb.stru03.Stru03Record());
        }

        public Stru03RecordJaxb(legstar.test.jaxb.stru03.Stru03Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComNumber((Long) value);
                break;
            case 1:
                getJaxb().setComName((String) value);
                break;
            case 2:
                getJaxb().setComAmount((java.math.BigDecimal) value);
                break;
            case 3:
                java.util.List < legstar.test.jaxb.stru03.ComArray > comArray = new java.util.ArrayList < legstar.test.jaxb.stru03.ComArray >();
                for (ComArrayJaxb wrapperItem : (java.util.List < ComArrayJaxb >) value) {
                    comArray.add(wrapperItem.getJaxb());
                }
                getJaxb().getComArray().addAll(comArray);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Stru03Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComNumber();
            case 1:
                return getJaxb().getComName();
            case 2:
                return getJaxb().getComAmount();
            case 3:
                java.util.List < ComArrayJaxb > comArray = new java.util.ArrayList < ComArrayJaxb >();
                for (legstar.test.jaxb.stru03.ComArray jaxbItem : getJaxb().getComArray()) {
                    comArray.add(new ComArrayJaxb(jaxbItem));
                }
                return comArray;
            default:
                throw new InvalidComplexTypeFieldIndex("Stru03Record", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comNumber=");
            builder.append(getJaxb().getComNumber());
            builder.append(", ");
            builder.append("comName=");
            builder.append(getJaxb().getComName());
            builder.append(", ");
            builder.append("comAmount=");
            builder.append(getJaxb().getComAmount());
            builder.append(", ");
            builder.append("comArray=");
            java.util.List < ComArrayJaxb > comArray = new java.util.ArrayList < ComArrayJaxb >();
            for (legstar.test.jaxb.stru03.ComArray jaxbItem : getJaxb().getComArray()) {
                comArray.add(new ComArrayJaxb(jaxbItem));
            }
            builder.append(comArray);
            builder.append("}");
            return builder.toString();
        }

    }

}
