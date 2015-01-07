package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef02RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.rdef02.Rdef02Record > {

    public Cob2Rdef02RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.rdef02.Rdef02Record >()
                .cobolComplexType(new CobolRdef02Record())
                .jaxbClass(legstar.test.jaxb.rdef02.Rdef02Record.class)
                .jaxbWrapperFactory(new Rdef02RecordJaxb()));
    }

}

