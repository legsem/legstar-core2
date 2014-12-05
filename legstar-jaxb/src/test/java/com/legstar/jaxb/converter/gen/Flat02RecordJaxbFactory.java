package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Flat02RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("Flat02Record".equals(type.getName())) {
            return new Flat02RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Flat02Record".equals(type.getName())) {
            return new Flat02RecordJaxb((legstar.test.jaxb.flat02.Flat02Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class Flat02RecordJaxb extends JaxbWrapper<legstar.test.jaxb.flat02.Flat02Record> {

        public Flat02RecordJaxb() {
            this(new legstar.test.jaxb.flat02.Flat02Record());
        }

        public Flat02RecordJaxb(legstar.test.jaxb.flat02.Flat02Record jaxb) {
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
                getJaxb().getComArray().addAll((java.util.List <Short>) value);
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
