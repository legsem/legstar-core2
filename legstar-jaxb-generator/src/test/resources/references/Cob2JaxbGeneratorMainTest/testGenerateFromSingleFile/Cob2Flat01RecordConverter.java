package flat01;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Flat01RecordConverter extends Cob2JaxbConverter < flat01.Flat01Record > {

    public Cob2Flat01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < flat01.Flat01Record >()
                .cobolComplexType(new CobolFlat01Record())
                .jaxbClass(flat01.Flat01Record.class)
                .jaxbWrapperFactory(new Flat01RecordJaxb()));
    }

}

