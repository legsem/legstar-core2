package legstar.samples.custdat;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2CustomerData extends Cob2Jaxb<legstar.samples.custdat.CustomerData> {

    public Cob2CustomerData() {
        super(new CobolCustomerData(), new CustomerDataJaxb(), CustomerData.class);
    }

}
