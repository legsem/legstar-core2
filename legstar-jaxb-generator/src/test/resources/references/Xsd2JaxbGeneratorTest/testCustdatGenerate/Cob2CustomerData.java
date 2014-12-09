package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2CustomerData extends Cob2Jaxb<legstar.test.jaxb.cusdat.CustomerData> {

    public Cob2CustomerData() {
        super(
            new CobolCustomerData(),
            new CustomerDataJaxb(),
            legstar.test.jaxb.cusdat.CustomerData.class
        );
    }

}
