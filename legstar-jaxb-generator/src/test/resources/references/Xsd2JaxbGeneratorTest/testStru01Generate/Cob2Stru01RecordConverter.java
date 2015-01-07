package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru01RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.stru01.Stru01Record > {

    public Cob2Stru01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.stru01.Stru01Record >()
                .cobolComplexType(new CobolStru01Record())
                .jaxbClass(legstar.test.jaxb.stru01.Stru01Record.class)
                .jaxbWrapperFactory(new Stru01RecordJaxb()));
    }

}

