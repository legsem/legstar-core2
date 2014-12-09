package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Flat02Record extends Cob2Jaxb<legstar.test.jaxb.flat02.Flat02Record> {

    public Cob2Flat02Record() {
        super(
            new CobolFlat02Record(),
            new Flat02RecordJaxb(),
            legstar.test.jaxb.flat02.Flat02Record.class
        );
    }

}
