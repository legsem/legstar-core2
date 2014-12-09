package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Flat01Record extends Cob2Jaxb<legstar.test.jaxb.flat01.Flat01Record> {

    public Cob2Flat01Record() {
        super(
            new CobolFlat01Record(),
            new Flat01RecordJaxb(),
            legstar.test.jaxb.flat01.Flat01Record.class
        );
    }

}
