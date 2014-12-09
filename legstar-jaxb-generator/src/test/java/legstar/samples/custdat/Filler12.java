
package legstar.samples.custdat;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour Filler12 complex type.
 * 
 * <p>Le fragment de sch�ma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="Filler12"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="transactionDay"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="2"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="filler14"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="1"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="transactionMonth"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="2"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="filler16"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="1"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="transactionYear"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="2"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler12", namespace = "http://samples.legstar/custdat", propOrder = {
    "transactionDay",
    "filler14",
    "transactionMonth",
    "filler16",
    "transactionYear"
})
public class Filler12 {

    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String transactionDay;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String filler14;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String transactionMonth;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String filler16;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String transactionYear;

    /**
     * Obtient la valeur de la propri�t� transactionDay.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionDay() {
        return transactionDay;
    }

    /**
     * D�finit la valeur de la propri�t� transactionDay.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionDay(String value) {
        this.transactionDay = value;
    }

    /**
     * Obtient la valeur de la propri�t� filler14.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller14() {
        return filler14;
    }

    /**
     * D�finit la valeur de la propri�t� filler14.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller14(String value) {
        this.filler14 = value;
    }

    /**
     * Obtient la valeur de la propri�t� transactionMonth.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionMonth() {
        return transactionMonth;
    }

    /**
     * D�finit la valeur de la propri�t� transactionMonth.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionMonth(String value) {
        this.transactionMonth = value;
    }

    /**
     * Obtient la valeur de la propri�t� filler16.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller16() {
        return filler16;
    }

    /**
     * D�finit la valeur de la propri�t� filler16.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller16(String value) {
        this.filler16 = value;
    }

    /**
     * Obtient la valeur de la propri�t� transactionYear.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionYear() {
        return transactionYear;
    }

    /**
     * D�finit la valeur de la propri�t� transactionYear.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionYear(String value) {
        this.transactionYear = value;
    }

}
