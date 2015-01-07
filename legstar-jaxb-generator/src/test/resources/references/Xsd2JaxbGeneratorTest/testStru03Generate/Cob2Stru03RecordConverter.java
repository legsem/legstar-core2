package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru03RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.stru03.Stru03Record > {

    public Cob2Stru03RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.stru03.Stru03Record >()
                .cobolComplexType(new CobolStru03Record())
                .jaxbClass(legstar.test.jaxb.stru03.Stru03Record.class)
                .jaxbWrapperFactory(new Stru03RecordJaxb()));
    }

}

