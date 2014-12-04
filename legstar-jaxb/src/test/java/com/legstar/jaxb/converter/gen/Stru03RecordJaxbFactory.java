package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

@SuppressWarnings("unchecked")
public class Stru03RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper < ? > create(CobolComplexType type) {
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordWrapper();
        } else if ("ComArray".equals(type.getName())) {
            return new ComArrayWrapper();
        } else {
            throw new InvalidComplexTypeName(type.getName());
        }
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordWrapper((legstar.test.jaxb.stru03.Stru03Record) jaxb);
        } else if ("ComArray".equals(type.getName())) {
            return new ComArrayWrapper((legstar.test.jaxb.stru03.ComArray) jaxb);
        } else {
            throw new InvalidComplexTypeName(type.getName());
        }
    }

    public class Stru03RecordWrapper extends JaxbWrapper < legstar.test.jaxb.stru03.Stru03Record > {

        public Stru03RecordWrapper() {
            this(new legstar.test.jaxb.stru03.Stru03Record());
        }

        public Stru03RecordWrapper(legstar.test.jaxb.stru03.Stru03Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {
            switch (index) {
            case 0:
                getJaxb().setComNumber(((Number) value).longValue());
                break;
            case 1:
                getJaxb().setComName((String) value);
                break;
            case 2:
                getJaxb().setComAmount((java.math.BigDecimal) value);
                break;
            case 3:
                getJaxb()
                        .getComArray()
                        .addAll(unwrapComArray((java.util.List < ComArrayWrapper >) value));
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Stru03Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return Long.valueOf(getJaxb().getComNumber());
            case 1:
                return getJaxb().getComName();
            case 2:
                return getJaxb().getComAmount();
            case 3:
                return wrapComArray(getJaxb().getComArray());
            default:
                throw new InvalidComplexTypeFieldIndex("Stru03Record", index);
            }
        }

        public java.util.List < legstar.test.jaxb.stru03.ComArray > unwrapComArray(
                java.util.List < ComArrayWrapper > wrapperArray) {
            java.util.List < legstar.test.jaxb.stru03.ComArray > jaxbArray = new java.util.ArrayList < legstar.test.jaxb.stru03.ComArray >();
            for (ComArrayWrapper wrapperItem : wrapperArray) {
                jaxbArray.add(wrapperItem.getJaxb());
            }
            return jaxbArray;
        }

        public java.util.List < ComArrayWrapper > wrapComArray(
                java.util.List < legstar.test.jaxb.stru03.ComArray > jaxbArray) {
            java.util.List < ComArrayWrapper > arrayWrapper = new java.util.ArrayList < ComArrayWrapper >();
            for (legstar.test.jaxb.stru03.ComArray jaxbItem : jaxbArray) {
                arrayWrapper.add(new ComArrayWrapper(jaxbItem));
            }
            return arrayWrapper;
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
            builder.append(wrapComArray(getJaxb().getComArray()));
            builder.append("}");
            return builder.toString();
        }

    }

    public class ComArrayWrapper extends JaxbWrapper < legstar.test.jaxb.stru03.ComArray > {

        public ComArrayWrapper() {
            this(new legstar.test.jaxb.stru03.ComArray());
        }

        public ComArrayWrapper(legstar.test.jaxb.stru03.ComArray jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {

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
                return Long.valueOf(getJaxb().getComItem1());
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
}
