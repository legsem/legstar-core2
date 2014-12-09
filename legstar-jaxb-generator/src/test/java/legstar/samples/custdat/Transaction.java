
package legstar.samples.custdat;

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour Transaction complex type.
 * 
 * <p>Le fragment de sch�ma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="Transaction"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;choice&gt;
 *           &lt;element name="transactionDate"&gt;
 *             &lt;simpleType&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *                 &lt;maxLength value="8"/&gt;
 *               &lt;/restriction&gt;
 *             &lt;/simpleType&gt;
 *           &lt;/element&gt;
 *           &lt;element name="filler12" type="{http://samples.legstar/custdat}Filler12"/&gt;
 *         &lt;/choice&gt;
 *         &lt;element name="transactionAmount"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="15"/&gt;
 *               &lt;fractionDigits value="2"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="transactionComment"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="9"/&gt;
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
@XmlType(name = "Transaction", namespace = "http://samples.legstar/custdat", propOrder = {
    "transactionDate",
    "filler12",
    "transactionAmount",
    "transactionComment"
})
public class Transaction {

    @XmlElement(namespace = "http://samples.legstar/custdat")
    protected String transactionDate;
    @XmlElement(namespace = "http://samples.legstar/custdat")
    protected Filler12 filler12;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected BigDecimal transactionAmount;
    @XmlElement(namespace = "http://samples.legstar/custdat", required = true)
    protected String transactionComment;

    /**
     * Obtient la valeur de la propri�t� transactionDate.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionDate() {
        return transactionDate;
    }

    /**
     * D�finit la valeur de la propri�t� transactionDate.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionDate(String value) {
        this.transactionDate = value;
    }

    /**
     * Obtient la valeur de la propri�t� filler12.
     * 
     * @return
     *     possible object is
     *     {@link Filler12 }
     *     
     */
    public Filler12 getFiller12() {
        return filler12;
    }

    /**
     * D�finit la valeur de la propri�t� filler12.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler12 }
     *     
     */
    public void setFiller12(Filler12 value) {
        this.filler12 = value;
    }

    /**
     * Obtient la valeur de la propri�t� transactionAmount.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getTransactionAmount() {
        return transactionAmount;
    }

    /**
     * D�finit la valeur de la propri�t� transactionAmount.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setTransactionAmount(BigDecimal value) {
        this.transactionAmount = value;
    }

    /**
     * Obtient la valeur de la propri�t� transactionComment.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionComment() {
        return transactionComment;
    }

    /**
     * D�finit la valeur de la propri�t� transactionComment.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionComment(String value) {
        this.transactionComment = value;
    }

}
