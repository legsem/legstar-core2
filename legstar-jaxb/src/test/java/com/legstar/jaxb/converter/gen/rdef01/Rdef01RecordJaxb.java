package com.legstar.jaxb.converter.gen.rdef01;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidChoiceAlternativeException;
import com.legstar.base.visitor.NoAlternativeForChoiceException;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndexException;
import com.legstar.base.visitor.InvalidComplexTypeNameException;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef01RecordJaxb implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper();
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper();
        }
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeNameException(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("ComDetail1".equals(type.getName())) {
            return new ComDetail1JaxbWrapper((legstar.test.jaxb.rdef01.ComDetail1) jaxb);
        }
        if ("ComDetail2".equals(type.getName())) {
            return new ComDetail2JaxbWrapper((legstar.test.jaxb.rdef01.ComDetail2) jaxb);
        }
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordJaxbWrapper((legstar.test.jaxb.rdef01.Rdef01Record) jaxb);
        }
        throw new InvalidComplexTypeNameException(type.getName());
    }

    public class ComDetail1JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail1> {

        public ComDetail1JaxbWrapper() {
            this(new legstar.test.jaxb.rdef01.ComDetail1());
        }

        public ComDetail1JaxbWrapper(legstar.test.jaxb.rdef01.ComDetail1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComName((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("ComDetail1", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComName();
            default:
                throw new InvalidComplexTypeFieldIndexException("ComDetail1", index);
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

    public class ComDetail2JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail2> {

        public ComDetail2JaxbWrapper() {
            this(new legstar.test.jaxb.rdef01.ComDetail2());
        }

        public ComDetail2JaxbWrapper(legstar.test.jaxb.rdef01.ComDetail2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setComAmount((java.math.BigDecimal) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("ComDetail2", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getComAmount();
            default:
                throw new InvalidComplexTypeFieldIndexException("ComDetail2", index);
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

    public class Rdef01RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.rdef01.Rdef01Record> {

        public Rdef01RecordJaxbWrapper() {
            this(new legstar.test.jaxb.rdef01.Rdef01Record());
        }

        public Rdef01RecordJaxbWrapper(legstar.test.jaxb.rdef01.Rdef01Record jaxb) {
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
                default:
                    throw new InvalidChoiceAlternativeException("ComDetail1Choice",
                            alternativeIndex);
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("Rdef01Record", index);
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
                } else {
                    throw new NoAlternativeForChoiceException("ComDetail1Choice");
                }
            default:
                throw new InvalidComplexTypeFieldIndexException("Rdef01Record", index);
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
            builder.append("}");
            return builder.toString();
        }

    }

}
