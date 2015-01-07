package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Flat02RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.flat02.Flat02Record > {

    public Cob2Flat02RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.flat02.Flat02Record >()
                .cobolComplexType(new CobolFlat02Record())
                .jaxbClass(legstar.test.jaxb.flat02.Flat02Record.class)
                .jaxbWrapperFactory(new Flat02RecordJaxb()));
    }

}

