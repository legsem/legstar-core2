package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Stru03RecordJaxbWrapperFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComArray".equals(type.getName())) {
            return new ComArrayJaxbWrapper();
        }
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComArray".equals(type.getName())) {
            return new ComArrayJaxbWrapper((legstar.test.jaxb.stru03.ComArray) jaxb);
        }
        if ("Stru03Record".equals(type.getName())) {
            return new Stru03RecordJaxbWrapper((legstar.test.jaxb.stru03.Stru03Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComArrayJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.stru03.ComArray> {

        public ComArrayJaxbWrapper() {
            this(new legstar.test.jaxb.stru03.ComArray());
        }

        public ComArrayJaxbWrapper(legstar.test.jaxb.stru03.ComArray jaxb) {
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
    public class Stru03RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.stru03.Stru03Record> {

        public Stru03RecordJaxbWrapper() {
            this(new legstar.test.jaxb.stru03.Stru03Record());
        }

        public Stru03RecordJaxbWrapper(legstar.test.jaxb.stru03.Stru03Record jaxb) {
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
                getJaxb().getComArray().clear();
                if (value instanceof java.util.List) {
                    for (Object wrapperItem : (java.util.List<?>) value) {
                        if (wrapperItem instanceof ComArrayJaxbWrapper){
                            getJaxb().getComArray().add(((ComArrayJaxbWrapper) wrapperItem).getJaxb());
                        }
                    }
                }
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
                java.util.List < ComArrayJaxbWrapper > comArray = new java.util.ArrayList < ComArrayJaxbWrapper >();
                for (legstar.test.jaxb.stru03.ComArray jaxbItem : getJaxb().getComArray()) {
                    comArray.add(new ComArrayJaxbWrapper(jaxbItem));
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
            java.util.List < ComArrayJaxbWrapper > comArray = new java.util.ArrayList < ComArrayJaxbWrapper >();
            for (legstar.test.jaxb.stru03.ComArray jaxbItem : getJaxb().getComArray()) {
                comArray.add(new ComArrayJaxbWrapper(jaxbItem));
            }
            builder.append(comArray);
            builder.append("}");
            return builder.toString();
        }

    }

}
