package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef03RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.rdef03.Rdef03Record > {

    public Cob2Rdef03RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.rdef03.Rdef03Record >()
                .cobolComplexType(new CobolRdef03Record())
                .jaxbClass(legstar.test.jaxb.rdef03.Rdef03Record.class)
                .jaxbWrapperFactory(new Rdef03RecordJaxb()));
    }

}

