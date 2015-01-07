package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2CustomerDataConverter extends Cob2JaxbConverter < legstar.test.jaxb.cusdat.CustomerData > {

    public Cob2CustomerDataConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.cusdat.CustomerData >()
                .cobolComplexType(new CobolCustomerData())
                .jaxbClass(legstar.test.jaxb.cusdat.CustomerData.class)
                .jaxbWrapperFactory(new CustomerDataJaxb()));
    }

}

