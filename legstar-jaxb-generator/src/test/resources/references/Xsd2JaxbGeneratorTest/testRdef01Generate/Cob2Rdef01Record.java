package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Rdef01Record extends Cob2Jaxb<legstar.test.jaxb.rdef01.Rdef01Record> {

    public Cob2Rdef01Record() {
        super(
            new CobolRdef01Record(),
            new Rdef01RecordJaxb(),
            legstar.test.jaxb.rdef01.Rdef01Record.class
        );
    }

}
