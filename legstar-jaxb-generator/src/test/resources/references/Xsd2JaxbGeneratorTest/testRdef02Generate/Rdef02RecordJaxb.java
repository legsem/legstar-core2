package test.example;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceTypeAlternative;
import com.legstar.base.visitor.NoAlternativeForChoiceType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef02RecordJaxb implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("Rdef02Key".equals(type.getName())) {
            return new Rdef02KeyJaxbWrapper();
        }
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper();
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper();
        }
        if ("Rdef02Record".equals(type.getName())) {
            return new Rdef02RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Rdef02Key".equals(type.getName())) {
            return new Rdef02KeyJaxbWrapper((legstar.test.jaxb.rdef02.Rdef02Key) jaxb);
        }
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper((legstar.test.jaxb.rdef02.ComDetail1) jaxb);
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper((legstar.test.jaxb.rdef02.ComDetail2) jaxb);
        }
        if ("Rdef02Record".equals(type.getName())) {
            return new Rdef02RecordJaxbWrapper((legstar.test.jaxb.rdef02.Rdef02Record) jaxb);
        }
        throw new InvalidComplexTypeName(type.getName());
    }

    public class Rdef02KeyJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef02.Rdef02Key> {

        public Rdef02KeyJaxbWrapper() {
            this(new legstar.test.jaxb.rdef02.Rdef02Key());
        }

        public Rdef02KeyJaxbWrapper(legstar.test.jaxb.rdef02.Rdef02Key jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                switch (alternativeIndex) {
                case 0:
                    getJaxb().setRdef02Item1((Long) value);
                    break;
                case 1:
                    getJaxb().setRdef02Item2((String) value);
                    break;
                default:
                    throw new InvalidChoiceTypeAlternative("Rdef02Item1Choice",
                            alternativeIndex);
                }
                break;
            case 1:
                getJaxb().setComSelect((Integer) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef02Key", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                if (getJaxb().getRdef02Item1() != null) {
                    return getJaxb().getRdef02Item1();
                } else if (getJaxb().getRdef02Item2() != null) {
                    return getJaxb().getRdef02Item2();
                } else {
                    throw new NoAlternativeForChoiceType("Rdef02Item1Choice");
                }
            case 1:
                return getJaxb().getComSelect();
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef02Key", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("rdef02Item1Choice=");
            if (getJaxb().getRdef02Item1() != null) {
                builder.append("rdef02Item1=");
                builder.append(getJaxb().getRdef02Item1());
            }
            if (getJaxb().getRdef02Item2() != null) {
                builder.append("rdef02Item2=");
                builder.append(getJaxb().getRdef02Item2());
            }
            builder.append(", ");
            builder.append("comSelect=");
            builder.append(getJaxb().getComSelect());
            builder.append("}");
            return builder.toString();
        }

    }

    public class ComDetail1JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef02.ComDetail1> {

        public ComDetail1JaxbWrapper() {
            this(new legstar.test.jaxb.rdef02.ComDetail1());
        }

        public ComDetail1JaxbWrapper(legstar.test.jaxb.rdef02.ComDetail1 jaxb) {
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

    public class ComDetail2JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef02.ComDetail2> {

        public ComDetail2JaxbWrapper() {
            this(new legstar.test.jaxb.rdef02.ComDetail2());
        }

        public ComDetail2JaxbWrapper(legstar.test.jaxb.rdef02.ComDetail2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComAmount((java.math.BigDecimal) value);
                break;
            case 1:
                getJaxb().setFiller13((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("ComDetail2", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComAmount();
            case 1:
                return getJaxb().getFiller13();
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
            builder.append(", ");
            builder.append("filler13=");
            builder.append(getJaxb().getFiller13());
            builder.append("}");
            return builder.toString();
        }

    }

    public class Rdef02RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef02.Rdef02Record> {

        public Rdef02RecordJaxbWrapper() {
            this(new legstar.test.jaxb.rdef02.Rdef02Record());
        }

        public Rdef02RecordJaxbWrapper(legstar.test.jaxb.rdef02.Rdef02Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setRdef02Key(((Rdef02KeyJaxbWrapper) value).getJaxb());
                break;
            case 1:
                switch (alternativeIndex) {
                case 0:
                    getJaxb().setComDetail1(((ComDetail1JaxbWrapper) value).getJaxb());
                    break;
                case 1:
                    getJaxb().setComDetail2(((ComDetail2JaxbWrapper) value).getJaxb());
                    break;
                default:
                    throw new InvalidChoiceTypeAlternative("ComDetail1Choice",
                            alternativeIndex);
                }
                break;
            case 2:
                getJaxb().setComItem3((java.math.BigDecimal) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef02Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return new Rdef02KeyJaxbWrapper(getJaxb().getRdef02Key());
            case 1:
                if (getJaxb().getComDetail1() != null) {
                    return new ComDetail1JaxbWrapper(getJaxb().getComDetail1());
                } else if (getJaxb().getComDetail2() != null) {
                    return new ComDetail2JaxbWrapper(getJaxb().getComDetail2());
                } else {
                    throw new NoAlternativeForChoiceType("ComDetail1Choice");
                }
            case 2:
                return getJaxb().getComItem3();
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef02Record", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("rdef02Key=");
            builder.append(new Rdef02KeyJaxbWrapper(getJaxb().getRdef02Key()));
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
            builder.append(", ");
            builder.append("comItem3=");
            builder.append(getJaxb().getComItem3());
            builder.append("}");
            return builder.toString();
        }

    }

}
