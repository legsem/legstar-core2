
package flat01;

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour Flat01Record complex type.
 * 
 * <p>Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="Flat01Record"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="comNumber"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt"&gt;
 *               &lt;totalDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="comName"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="20"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="comAmount"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="7"/&gt;
 *               &lt;fractionDigits value="2"/&gt;
 *               &lt;minInclusive value="0"/&gt;
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
@XmlType(name = "Flat01Record", namespace = "http://flat01", propOrder = {
    "comNumber",
    "comName",
    "comAmount"
})
public class Flat01Record {

    @XmlElement(namespace = "http://flat01")
    protected long comNumber;
    @XmlElement(namespace = "http://flat01", required = true)
    protected String comName;
    @XmlElement(namespace = "http://flat01", required = true)
    protected BigDecimal comAmount;

    /**
     * Obtient la valeur de la propriété comNumber.
     * 
     */
    public long getComNumber() {
        return comNumber;
    }

    /**
     * Définit la valeur de la propriété comNumber.
     * 
     */
    public void setComNumber(long value) {
        this.comNumber = value;
    }

    /**
     * Obtient la valeur de la propriété comName.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComName() {
        return comName;
    }

    /**
     * Définit la valeur de la propriété comName.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComName(String value) {
        this.comName = value;
    }

    /**
     * Obtient la valeur de la propriété comAmount.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getComAmount() {
        return comAmount;
    }

    /**
     * Définit la valeur de la propriété comAmount.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setComAmount(BigDecimal value) {
        this.comAmount = value;
    }

}
