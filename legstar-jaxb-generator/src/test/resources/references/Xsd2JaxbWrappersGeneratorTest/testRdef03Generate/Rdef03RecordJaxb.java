package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceTypeAlternative;
import com.legstar.base.visitor.NoAlternativeForChoiceType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef03RecordJaxb implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper();
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper();
        }
        if ("ComDetail3".equals(type.getName())) {
            return new ComDetail3JaxbWrapper();
        }
        if ("Rdef03Record".equals(type.getName())) {
            return new Rdef03RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper((legstar.test.jaxb.rdef03.ComDetail1) jaxb);
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper((legstar.test.jaxb.rdef03.ComDetail2) jaxb);
        }
        if ("ComDetail3".equals(type.getName())) {
            return new ComDetail3JaxbWrapper((legstar.test.jaxb.rdef03.ComDetail3) jaxb);
        }
        if ("Rdef03Record".equals(type.getName())) {
            return new Rdef03RecordJaxbWrapper((legstar.test.jaxb.rdef03.Rdef03Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComDetail1JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail1> {

        public ComDetail1JaxbWrapper() {
            this(new legstar.test.jaxb.rdef03.ComDetail1());
        }

        public ComDetail1JaxbWrapper(legstar.test.jaxb.rdef03.ComDetail1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComName((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail1", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComName();
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail1", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comName=");
            builder.append(getJaxb().getComName());
            builder.append("}");
            return builder.toString();
        }

    }
    public class ComDetail2JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail2> {

        public ComDetail2JaxbWrapper() {
            this(new legstar.test.jaxb.rdef03.ComDetail2());
        }

        public ComDetail2JaxbWrapper(legstar.test.jaxb.rdef03.ComDetail2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComAmount((java.math.BigDecimal) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail2", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComAmount();
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail2", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comAmount=");
            builder.append(getJaxb().getComAmount());
            builder.append("}");
            return builder.toString();
        }

    }
    public class ComDetail3JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail3> {

        public ComDetail3JaxbWrapper() {
            this(new legstar.test.jaxb.rdef03.ComDetail3());
        }

        public ComDetail3JaxbWrapper(legstar.test.jaxb.rdef03.ComDetail3 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComNumber((Long) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail3", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComNumber();
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail3", index);
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
            builder.append("}");
            return builder.toString();
        }

    }
    public class Rdef03RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef03.Rdef03Record> {

        public Rdef03RecordJaxbWrapper() {
            this(new legstar.test.jaxb.rdef03.Rdef03Record());
        }

        public Rdef03RecordJaxbWrapper(legstar.test.jaxb.rdef03.Rdef03Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComSelect((Integer) value);
                break;
            case 1:
                switch (alternativeIndex) {
                case 0:
                    getJaxb().setComDetail1(((ComDetail1JaxbWrapper) value).getJaxb());
                    break;
                case 1:
                    getJaxb().setComDetail2(((ComDetail2JaxbWrapper) value).getJaxb());
                    break;
                case 2:
                    getJaxb().setComDetail3(((ComDetail3JaxbWrapper) value).getJaxb());
                    break;
                default:
                    throw new InvalidChoiceTypeAlternative("ComDetail1Choice",
                            alternativeIndex);
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef03Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComSelect();
            case 1:
                if (getJaxb().getComDetail1() != null) {
                    return new ComDetail1JaxbWrapper(getJaxb().getComDetail1());
                } else if (getJaxb().getComDetail2() != null) {
                    return new ComDetail2JaxbWrapper(getJaxb().getComDetail2());
                } else if (getJaxb().getComDetail3() != null) {
                    return new ComDetail3JaxbWrapper(getJaxb().getComDetail3());
                } else {
                    throw new NoAlternativeForChoiceType("ComDetail1Choice");
                }
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef03Record", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("comSelect=");
            builder.append(getJaxb().getComSelect());
            builder.append(", ");
            builder.append("comDetail1Choice=");
            if (getJaxb().getComDetail1() != null) {
                builder.append("comDetail1=");
                builder.append(new ComDetail1JaxbWrapper(getJaxb().getComDetail1()));
            }
            if (getJaxb().getComDetail2() != null) {
                builder.append("comDetail2=");
                builder.append(new ComDetail2JaxbWrapper(getJaxb().getComDetail2()));
            }
            if (getJaxb().getComDetail3() != null) {
                builder.append("comDetail3=");
                builder.append(new ComDetail3JaxbWrapper(getJaxb().getComDetail3()));
            }
            builder.append("}");
            return builder.toString();
        }

    }

}
