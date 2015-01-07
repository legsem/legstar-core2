package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Ardo01RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.ardo01.Ardo01Record > {

    public Cob2Ardo01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.ardo01.Ardo01Record >()
                .cobolComplexType(new CobolArdo01Record())
                .jaxbClass(legstar.test.jaxb.ardo01.Ardo01Record.class)
                .jaxbWrapperFactory(new Ardo01RecordJaxb()));
    }

}

