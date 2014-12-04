package com.legstar.jaxb.converter.gen;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndex;
import com.legstar.base.visitor.InvalidComplexTypeName;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Rdef01RecordJaxbFactory implements JaxbWrapperFactory {

    public JaxbWrapper < ? > create(CobolComplexType type) {
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordWrapper();
        } else if("ComDetail1".equals(type.getName())) {
            return new ComDetail1Wrapper();
        } else if("ComDetail2".equals(type.getName())) {
            return new ComDetail2Wrapper();
        } else {
            throw new InvalidComplexTypeName(type.getName());
        }
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("Rdef01Record".equals(type.getName())) {
            return new Rdef01RecordWrapper(
                    (legstar.test.jaxb.rdef01.Rdef01Record) jaxb);
        } else if("ComDetail1".equals(type.getName())) {
            return new ComDetail1Wrapper();
        } else if("ComDetail2".equals(type.getName())) {
            return new ComDetail2Wrapper();
        } else {
            throw new InvalidComplexTypeName(type.getName());
        }
    }

    public class Rdef01RecordWrapper extends
            JaxbWrapper < legstar.test.jaxb.rdef01.Rdef01Record > {

        public Rdef01RecordWrapper() {
            this(new legstar.test.jaxb.rdef01.Rdef01Record());
        }

        public Rdef01RecordWrapper(legstar.test.jaxb.rdef01.Rdef01Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {

            switch (index) {
            case 0:
                getJaxb().setComSelect(((Number) value).intValue());
                break;
            case 1:
                if (value instanceof ComDetail1Wrapper) {
                    getJaxb().setComDetail1(((ComDetail1Wrapper) value).getJaxb());
                } else if(value instanceof ComDetail2Wrapper) {
                    getJaxb().setComDetail2(((ComDetail2Wrapper) value).getJaxb());
                } else {
                    throw new InvalidComplexTypeFieldIndex("Rdef01Record", index);
                }
                break;
            default:
                throw new InvalidComplexTypeFieldIndex("Rdef01Record", index);
            }

        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return Integer.valueOf(getJaxb().getComSelect());
            case 1:
                if (getJaxb().getComDetail1() != null) {
                    return new ComDetail1Wrapper(getJaxb().getComDetail1());
                } else if(getJaxb().getComDetail2() != null) {
                    return new ComDetail2Wrapper(getJaxb().getComDetail2());
                } else {
                    throw new InvalidComplexTypeFieldIndex("Rdef01Record", index);
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
            builder.append("comDetail1=");
            builder.append(new ComDetail1Wrapper(getJaxb().getComDetail1()));
            builder.append(", ");
            builder.append("comDetail2=");
            builder.append(new ComDetail2Wrapper(getJaxb().getComDetail2()));
            builder.append("}");
            return builder.toString();
        }

    }

    public class ComDetail1Wrapper extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail1> {

        public ComDetail1Wrapper() {
            this(new legstar.test.jaxb.rdef01.ComDetail1());
        }

        public ComDetail1Wrapper(legstar.test.jaxb.rdef01.ComDetail1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {

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


    public class ComDetail2Wrapper extends JaxbWrapper<legstar.test.jaxb.rdef01.ComDetail2> {

        public ComDetail2Wrapper() {
            this(new legstar.test.jaxb.rdef01.ComDetail2());
        }

        public ComDetail2Wrapper(legstar.test.jaxb.rdef01.ComDetail2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value) {

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

}
