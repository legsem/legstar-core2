
package legstar.samples.custdat;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour CustomerData complex type.
 * 
 * <p>Le fragment de sch�ma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="CustomerData"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="customerId"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt"&gt;
 *               &lt;totalDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="personalData" type="{http://samples.legstar/custdat}PersonalData"/&gt;
 *         &lt;element name="transactions" type="{http://samples.legstar/custdat}Transactions"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CustomerData", namespace = "http://samples.legstar/custdat", propOrder = {
    "customerId",
    "personalData",
    "transactions"
})
public class CustomerData {

    @XmlElement(namespace = "http://samples.legstar/custdat")
    protected long customerId;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected PersonalData personalData;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected Transactions transactions;

    /**
     * Obtient la valeur de la propri�t� customerId.
     * 
     */
    public long getCustomerId() {
        return customerId;
    }

    /**
     * D�finit la valeur de la propri�t� customerId.
     * 
     */
    public void setCustomerId(long value) {
        this.customerId = value;
    }

    /**
     * Obtient la valeur de la propri�t� personalData.
     * 
     * @return
     *     possible object is
     *     {@link PersonalData }
     *     
     */
    public PersonalData getPersonalData() {
        return personalData;
    }

    /**
     * D�finit la valeur de la propri�t� personalData.
     * 
     * @param value
     *     allowed object is
     *     {@link PersonalData }
     *     
     */
    public void setPersonalData(PersonalData value) {
        this.personalData = value;
    }

    /**
     * Obtient la valeur de la propri�t� transactions.
     * 
     * @return
     *     possible object is
     *     {@link Transactions }
     *     
     */
    public Transactions getTransactions() {
        return transactions;
    }

    /**
     * D�finit la valeur de la propri�t� transactions.
     * 
     * @param value
     *     allowed object is
     *     {@link Transactions }
     *     
     */
    public void setTransactions(Transactions value) {
        this.transactions = value;
    }

}
