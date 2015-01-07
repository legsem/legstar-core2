package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru04RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.stru04.Stru04Record > {

    public Cob2Stru04RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.stru04.Stru04Record >()
                .cobolComplexType(new CobolStru04Record())
                .jaxbClass(legstar.test.jaxb.stru04.Stru04Record.class)
                .jaxbWrapperFactory(new Stru04RecordJaxb()));
    }

}

