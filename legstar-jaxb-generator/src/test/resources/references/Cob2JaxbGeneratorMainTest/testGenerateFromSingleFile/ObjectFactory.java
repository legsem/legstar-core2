
package flat01;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the flat01 package. 
 * &lt;p&gt;An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Flat01Record_QNAME = new QName("http://flat01", "flat01Record");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: flat01
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Flat01Record }
     * 
     */
    public Flat01Record createFlat01Record() {
        return new Flat01Record();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Flat01Record }{@code >}
     * 
     * @param value
     *     Java instance representing xml element's value.
     * @return
     *     the new instance of {@link JAXBElement }{@code <}{@link Flat01Record }{@code >}
     */
    @XmlElementDecl(namespace = "http://flat01", name = "flat01Record")
    public JAXBElement<Flat01Record> createFlat01Record(Flat01Record value) {
        return new JAXBElement<Flat01Record>(_Flat01Record_QNAME, Flat01Record.class, null, value);
    }

}
