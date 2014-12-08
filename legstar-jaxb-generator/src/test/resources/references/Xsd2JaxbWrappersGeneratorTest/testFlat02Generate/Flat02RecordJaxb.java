package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Flat02RecordJaxb implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("Flat02Record".equals(type.getName())) {
            return new Flat02RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Flat02Record".equals(type.getName())) {
            return new Flat02RecordJaxbWrapper((legstar.test.jaxb.flat02.Flat02Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class Flat02RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.flat02.Flat02Record> {

        public Flat02RecordJaxbWrapper() {
            this(new legstar.test.jaxb.flat02.Flat02Record());
        }

        public Flat02RecordJaxbWrapper(legstar.test.jaxb.flat02.Flat02Record jaxb) {
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
                    for (Object item : (java.util.List<?>) value) {
                        if (item instanceof Short){
                            getJaxb().getComArray().add((Short) item);
                        }
                    }
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Flat02Record", index);
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
                return getJaxb().getComArray();
            default:
                throw new InvalidComplexTypeFieldIndex("Flat02Record", index);
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
            builder.append(getJaxb().getComArray());
            builder.append("}");
            return builder.toString();
        }

    }

}
