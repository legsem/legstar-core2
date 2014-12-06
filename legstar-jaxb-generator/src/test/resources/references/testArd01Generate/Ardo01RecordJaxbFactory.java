package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Ardo01RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("Ardo01Record".equals(type.getName())) {
            return new Ardo01RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Ardo01Record".equals(type.getName())) {
            return new Ardo01RecordJaxb((legstar.test.jaxb.ardo01.Ardo01Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class Ardo01RecordJaxb extends JaxbWrapper<legstar.test.jaxb.ardo01.Ardo01Record> {

        public Ardo01RecordJaxb() {
            this(new legstar.test.jaxb.ardo01.Ardo01Record());
        }

        public Ardo01RecordJaxb(legstar.test.jaxb.ardo01.Ardo01Record jaxb) {
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
                getJaxb().setComNbr((Integer) value);
                break;
            case 3:
                getJaxb().getComArray().clear();
                if (value instanceof java.util.List) {
                    for (Object item : (java.util.List<?>) value) {
                        if (item instanceof java.math.BigDecimal){
                            getJaxb().getComArray().add((java.math.BigDecimal) item);
                        }
                    }
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Ardo01Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComNumber();
            case 1:
                return getJaxb().getComName();
            case 2:
                return getJaxb().getComNbr();
            case 3:
                return getJaxb().getComArray();
            default:
                throw new InvalidComplexTypeFieldIndex("Ardo01Record", index);
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
            builder.append("comNbr=");
            builder.append(getJaxb().getComNbr());
            builder.append(", ");
            builder.append("comArray=");
            builder.append(getJaxb().getComArray());
            builder.append("}");
            return builder.toString();
        }

    }

}
