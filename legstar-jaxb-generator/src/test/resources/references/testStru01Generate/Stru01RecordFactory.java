package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Stru01RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComSubRecord".equals(type.getName())) {
            return new ComSubRecordJaxb();
        }
        if ("Stru01Record".equals(type.getName())) {
            return new Stru01RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComSubRecord".equals(type.getName())) {
            return new ComSubRecordJaxb((legstar.test.jaxb.stru01.ComSubRecord) jaxb);
        }
        if ("Stru01Record".equals(type.getName())) {
            return new Stru01RecordJaxb((legstar.test.jaxb.stru01.Stru01Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComSubRecordJaxb extends JaxbWrapper<legstar.test.jaxb.stru01.ComSubRecord> {

        public ComSubRecordJaxb() {
            this(new legstar.test.jaxb.stru01.ComSubRecord());
        }

        public ComSubRecordJaxb(legstar.test.jaxb.stru01.ComSubRecord jaxb) {
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
                throw new InvalidComplexTypeFieldIndex("ComSubRecord", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComItem1();
            case 1:
                return getJaxb().getComItem2();
            default:
                throw new InvalidComplexTypeFieldIndex("ComSubRecord", index);
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
    public class Stru01RecordJaxb extends JaxbWrapper<legstar.test.jaxb.stru01.Stru01Record> {

        public Stru01RecordJaxb() {
            this(new legstar.test.jaxb.stru01.Stru01Record());
        }

        public Stru01RecordJaxb(legstar.test.jaxb.stru01.Stru01Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {
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
                getJaxb().setComSubRecord(((ComSubRecordJaxb) value).getJaxb());
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Stru01Record", index);
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
                return new ComSubRecordJaxb(getJaxb().getComSubRecord());
            default:
                throw new InvalidComplexTypeFieldIndex("Stru01Record", index);
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
            builder.append("comSubRecord=");
            builder.append(new ComSubRecordJaxb(getJaxb().getComSubRecord()));
            builder.append("}");
            return builder.toString();
        }

    }

}
