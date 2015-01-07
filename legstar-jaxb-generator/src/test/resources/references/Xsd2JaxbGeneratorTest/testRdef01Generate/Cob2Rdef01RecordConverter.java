package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef01RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.rdef01.Rdef01Record > {

    public Cob2Rdef01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.rdef01.Rdef01Record >()
                .cobolComplexType(new CobolRdef01Record())
                .jaxbClass(legstar.test.jaxb.rdef01.Rdef01Record.class)
                .jaxbWrapperFactory(new Rdef01RecordJaxb()));
    }

}

