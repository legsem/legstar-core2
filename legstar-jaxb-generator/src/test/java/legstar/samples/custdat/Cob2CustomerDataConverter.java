package legstar.samples.custdat;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2CustomerDataConverter extends Cob2JaxbConverter < CustomerData > {

    public Cob2CustomerDataConverter() {
        super(new Cob2JaxbConverter.Builder < CustomerData >()
                .cobolComplexType(new CobolCustomerData())
                .jaxbClass(CustomerData.class)
                .jaxbWrapperFactory(new CustomerDataJaxb()));
    }

}
