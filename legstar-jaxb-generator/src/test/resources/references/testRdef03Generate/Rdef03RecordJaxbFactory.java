package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceTypeAlternative;
import com.legstar.base.visitor.NoAlternativeForChoiceType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef03RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1Jaxb();
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2Jaxb();
        }
        if ("ComDetail3".equals(type.getName())) {
            return new ComDetail3Jaxb();
        }
        if ("Rdef03Record".equals(type.getName())) {
            return new Rdef03RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1Jaxb((legstar.test.jaxb.rdef03.ComDetail1) jaxb);
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2Jaxb((legstar.test.jaxb.rdef03.ComDetail2) jaxb);
        }
        if ("ComDetail3".equals(type.getName())) {
            return new ComDetail3Jaxb((legstar.test.jaxb.rdef03.ComDetail3) jaxb);
        }
        if ("Rdef03Record".equals(type.getName())) {
            return new Rdef03RecordJaxb((legstar.test.jaxb.rdef03.Rdef03Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComDetail1Jaxb extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail1> {

        public ComDetail1Jaxb() {
            this(new legstar.test.jaxb.rdef03.ComDetail1());
        }

        public ComDetail1Jaxb(legstar.test.jaxb.rdef03.ComDetail1 jaxb) {
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
    public class ComDetail2Jaxb extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail2> {

        public ComDetail2Jaxb() {
            this(new legstar.test.jaxb.rdef03.ComDetail2());
        }

        public ComDetail2Jaxb(legstar.test.jaxb.rdef03.ComDetail2 jaxb) {
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
    public class ComDetail3Jaxb extends JaxbWrapper<legstar.test.jaxb.rdef03.ComDetail3> {

        public ComDetail3Jaxb() {
            this(new legstar.test.jaxb.rdef03.ComDetail3());
        }

        public ComDetail3Jaxb(legstar.test.jaxb.rdef03.ComDetail3 jaxb) {
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
    public class Rdef03RecordJaxb extends JaxbWrapper<legstar.test.jaxb.rdef03.Rdef03Record> {

        public Rdef03RecordJaxb() {
            this(new legstar.test.jaxb.rdef03.Rdef03Record());
        }

        public Rdef03RecordJaxb(legstar.test.jaxb.rdef03.Rdef03Record jaxb) {
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
                    getJaxb().setComDetail1(((ComDetail1Jaxb) value).getJaxb());
                    break;
                case 1:
                    getJaxb().setComDetail2(((ComDetail2Jaxb) value).getJaxb());
                    break;
                case 2:
                    getJaxb().setComDetail3(((ComDetail3Jaxb) value).getJaxb());
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
                    return new ComDetail1Jaxb(getJaxb().getComDetail1());
                } else if (getJaxb().getComDetail2() != null) {
                    return new ComDetail2Jaxb(getJaxb().getComDetail2());
                } else if (getJaxb().getComDetail3() != null) {
                    return new ComDetail3Jaxb(getJaxb().getComDetail3());
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
                builder.append(new ComDetail1Jaxb(getJaxb().getComDetail1()));
            }
            if (getJaxb().getComDetail2() != null) {
                builder.append("comDetail2=");
                builder.append(new ComDetail2Jaxb(getJaxb().getComDetail2()));
            }
            if (getJaxb().getComDetail3() != null) {
                builder.append("comDetail3=");
                builder.append(new ComDetail3Jaxb(getJaxb().getComDetail3()));
            }
            builder.append("}");
            return builder.toString();
        }

    }

}
