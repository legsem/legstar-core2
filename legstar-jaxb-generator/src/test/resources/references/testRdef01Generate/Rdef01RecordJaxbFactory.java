package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceTypeAlternative;
import com.legstar.base.visitor.NoAlternativeForChoiceType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef01RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1Jaxb();
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2Jaxb();
        }
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordJaxb();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1Jaxb((legstar.test.jaxb.rdef01.ComDetail1) jaxb);
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2Jaxb((legstar.test.jaxb.rdef01.ComDetail2) jaxb);
        }
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordJaxb((legstar.test.jaxb.rdef01.Rdef01Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class ComDetail1Jaxb extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail1> {

        public ComDetail1Jaxb() {
            this(new legstar.test.jaxb.rdef01.ComDetail1());
        }

        public ComDetail1Jaxb(legstar.test.jaxb.rdef01.ComDetail1 jaxb) {
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
    public class ComDetail2Jaxb extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail2> {

        public ComDetail2Jaxb() {
            this(new legstar.test.jaxb.rdef01.ComDetail2());
        }

        public ComDetail2Jaxb(legstar.test.jaxb.rdef01.ComDetail2 jaxb) {
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
    public class Rdef01RecordJaxb extends JaxbWrapper<legstar.test.jaxb.rdef01.Rdef01Record> {

        public Rdef01RecordJaxb() {
            this(new legstar.test.jaxb.rdef01.Rdef01Record());
        }

        public Rdef01RecordJaxb(legstar.test.jaxb.rdef01.Rdef01Record jaxb) {
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
                default:
                    throw new InvalidChoiceTypeAlternative("ComDetail1Choice",
                            alternativeIndex);
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef01Record", index);
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
                } else {
                    throw new NoAlternativeForChoiceType("ComDetail1Choice");
                }
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef01Record", index);
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
            builder.append("}");
            return builder.toString();
        }

    }

}
