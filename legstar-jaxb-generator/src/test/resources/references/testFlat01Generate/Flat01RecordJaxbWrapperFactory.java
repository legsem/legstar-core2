package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Flat01RecordJaxbWrapperFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("Flat01Record".equals(type.getName())) {
            return new Flat01RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Flat01Record".equals(type.getName())) {
            return new Flat01RecordJaxbWrapper((legstar.test.jaxb.flat01.Flat01Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class Flat01RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.flat01.Flat01Record> {

        public Flat01RecordJaxbWrapper() {
            this(new legstar.test.jaxb.flat01.Flat01Record());
        }

        public Flat01RecordJaxbWrapper(legstar.test.jaxb.flat01.Flat01Record jaxb) {
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
            default:
                throw new InvalidComplexTypeFieldIndex("Flat01Record", index);
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
            default:
                throw new InvalidComplexTypeFieldIndex("Flat01Record", index);
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
            builder.append("}");
            return builder.toString();
        }

    }

}
