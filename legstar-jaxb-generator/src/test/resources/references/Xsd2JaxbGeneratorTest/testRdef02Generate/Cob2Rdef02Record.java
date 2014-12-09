package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Rdef02Record extends Cob2Jaxb<legstar.test.jaxb.rdef02.Rdef02Record> {

    public Cob2Rdef02Record() {
        super(
            new CobolRdef02Record(),
            new Rdef02RecordJaxb(),
            legstar.test.jaxb.rdef02.Rdef02Record.class
        );
    }

}
