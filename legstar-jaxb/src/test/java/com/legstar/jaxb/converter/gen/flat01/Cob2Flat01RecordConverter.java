package com.legstar.jaxb.converter.gen.flat01;

import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Flat01RecordConverter extends Cob2JaxbConverter < legstar.test.jaxb.flat01.Flat01Record > {

    public Cob2Flat01RecordConverter() {
        super(new Cob2JaxbConverter.Builder < legstar.test.jaxb.flat01.Flat01Record >()
                .cobolComplexType(new legstar.test.jaxb.flat01.CobolFlat01Record())
                .jaxbClass(legstar.test.jaxb.flat01.Flat01Record.class)
                .jaxbWrapperFactory(new Flat01RecordJaxb()));
    }

}

