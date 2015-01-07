package test.example;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Cflt01RecordConverter extends Cob2JaxbConverter < test.example.Cflt01Record > {

    public Cob2Cflt01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < test.example.Cflt01Record >()
                .cobolComplexType(new CobolCflt01Record())
                .jaxbClass(test.example.Cflt01Record.class)
                .jaxbWrapperFactory(new Cflt01RecordJaxb()));
    }

}

