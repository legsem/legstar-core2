package com.legstar.jaxb.converter.gen.cflt01;

import com.legstar.base.type.composite.CobolComplexType;
import com.legstar.base.visitor.InvalidComplexTypeFieldIndexException;
import com.legstar.base.visitor.InvalidComplexTypeNameException;
import com.legstar.jaxb.converter.JaxbWrapper;
import com.legstar.jaxb.converter.JaxbWrapperFactory;

public class Cflt01RecordJaxb implements JaxbWrapperFactory {

    public JaxbWrapper<?> create(CobolComplexType type) {
        if ("CfltInfo9".equals(type.getName())) {
            return new CfltInfo9JaxbWrapper();
        }
        if ("CfltParent1".equals(type.getName())) {
            return new CfltParent1JaxbWrapper();
        }
        if ("CfltInfo13".equals(type.getName())) {
            return new CfltInfo13JaxbWrapper();
        }
        if ("CfltParent2".equals(type.getName())) {
            return new CfltParent2JaxbWrapper();
        }
        if ("Cflt01Record".equals(type.getName())) {
            return new Cflt01RecordJaxbWrapper();
        }
        throw new InvalidComplexTypeNameException(type.getName());
    }

    public JaxbWrapper < ? > create(CobolComplexType type, Object jaxb) {
        if ("CfltInfo9".equals(type.getName())) {
            return new CfltInfo9JaxbWrapper((legstar.test.jaxb.cflt01.CfltInfo9) jaxb);
        }
        if ("CfltParent1".equals(type.getName())) {
            return new CfltParent1JaxbWrapper((legstar.test.jaxb.cflt01.CfltParent1) jaxb);
        }
        if ("CfltInfo13".equals(type.getName())) {
            return new CfltInfo13JaxbWrapper((legstar.test.jaxb.cflt01.CfltInfo13) jaxb);
        }
        if ("CfltParent2".equals(type.getName())) {
            return new CfltParent2JaxbWrapper((legstar.test.jaxb.cflt01.CfltParent2) jaxb);
        }
        if ("Cflt01Record".equals(type.getName())) {
            return new Cflt01RecordJaxbWrapper((legstar.test.jaxb.cflt01.Cflt01Record) jaxb);
        }
        throw new InvalidComplexTypeNameException(type.getName());
    }

    public class CfltInfo9JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.cflt01.CfltInfo9> {

        public CfltInfo9JaxbWrapper() {
            this(new legstar.test.jaxb.cflt01.CfltInfo9());
        }

        public CfltInfo9JaxbWrapper(legstar.test.jaxb.cflt01.CfltInfo9 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCfltId((String) value);
                break;
            case 1:
                getJaxb().setCfltTypCd((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltInfo9", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getCfltId();
            case 1:
                return getJaxb().getCfltTypCd();
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltInfo9", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("cfltId=");
            builder.append(getJaxb().getCfltId());
            builder.append(", ");
            builder.append("cfltTypCd=");
            builder.append(getJaxb().getCfltTypCd());
            builder.append("}");
            return builder.toString();
        }

    }
    public class CfltParent1JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.cflt01.CfltParent1> {

        public CfltParent1JaxbWrapper() {
            this(new legstar.test.jaxb.cflt01.CfltParent1());
        }

        public CfltParent1JaxbWrapper(legstar.test.jaxb.cflt01.CfltParent1 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCfltInfo(((CfltInfo9JaxbWrapper) value).getJaxb());
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltParent1", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return new CfltInfo9JaxbWrapper(getJaxb().getCfltInfo());
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltParent1", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("cfltInfo=");
            builder.append(new CfltInfo9JaxbWrapper(getJaxb().getCfltInfo()));
            builder.append("}");
            return builder.toString();
        }

    }
    public class CfltInfo13JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.cflt01.CfltInfo13> {

        public CfltInfo13JaxbWrapper() {
            this(new legstar.test.jaxb.cflt01.CfltInfo13());
        }

        public CfltInfo13JaxbWrapper(legstar.test.jaxb.cflt01.CfltInfo13 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCfltIdCt((String) value);
                break;
            case 1:
                getJaxb().setCfltTypCdCt((String) value);
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltInfo13", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return getJaxb().getCfltIdCt();
            case 1:
                return getJaxb().getCfltTypCdCt();
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltInfo13", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("cfltIdCt=");
            builder.append(getJaxb().getCfltIdCt());
            builder.append(", ");
            builder.append("cfltTypCdCt=");
            builder.append(getJaxb().getCfltTypCdCt());
            builder.append("}");
            return builder.toString();
        }

    }
    public class CfltParent2JaxbWrapper extends JaxbWrapper<legstar.test.jaxb.cflt01.CfltParent2> {

        public CfltParent2JaxbWrapper() {
            this(new legstar.test.jaxb.cflt01.CfltParent2());
        }

        public CfltParent2JaxbWrapper(legstar.test.jaxb.cflt01.CfltParent2 jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCfltInfo(((CfltInfo13JaxbWrapper) value).getJaxb());
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltParent2", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return new CfltInfo13JaxbWrapper(getJaxb().getCfltInfo());
            default:
                throw new InvalidComplexTypeFieldIndexException("CfltParent2", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("cfltInfo=");
            builder.append(new CfltInfo13JaxbWrapper(getJaxb().getCfltInfo()));
            builder.append("}");
            return builder.toString();
        }

    }
    public class Cflt01RecordJaxbWrapper extends JaxbWrapper<legstar.test.jaxb.cflt01.Cflt01Record> {

        public Cflt01RecordJaxbWrapper() {
            this(new legstar.test.jaxb.cflt01.Cflt01Record());
        }

        public Cflt01RecordJaxbWrapper(legstar.test.jaxb.cflt01.Cflt01Record jaxb) {
            super(jaxb);
        }

        public void set(int index, Object value, int alternativeIndex) {
            switch (index) {
            case 0:
                getJaxb().setCfltParent1(((CfltParent1JaxbWrapper) value).getJaxb());
                break;
            case 1:
                getJaxb().setCfltParent2(((CfltParent2JaxbWrapper) value).getJaxb());
                break;
            default:
                throw new InvalidComplexTypeFieldIndexException("Cflt01Record", index);
            }
        }

        public Object get(int index) {
            switch (index) {
            case 0:
                return new CfltParent1JaxbWrapper(getJaxb().getCfltParent1());
            case 1:
                return new CfltParent2JaxbWrapper(getJaxb().getCfltParent2());
            default:
                throw new InvalidComplexTypeFieldIndexException("Cflt01Record", index);
            }
        }

        @Override
        public String toString() {
            if (getJaxb() == null) {
                return "{}";
            }
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            builder.append("cfltParent1=");
            builder.append(new CfltParent1JaxbWrapper(getJaxb().getCfltParent1()));
            builder.append(", ");
            builder.append("cfltParent2=");
            builder.append(new CfltParent2JaxbWrapper(getJaxb().getCfltParent2()));
            builder.append("}");
            return builder.toString();
        }

    }
}
