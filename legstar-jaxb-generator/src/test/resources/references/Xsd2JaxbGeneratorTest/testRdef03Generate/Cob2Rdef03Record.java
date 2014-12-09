package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Rdef03Record extends Cob2Jaxb<legstar.test.jaxb.rdef03.Rdef03Record> {

    public Cob2Rdef03Record() {
        super(
            new CobolRdef03Record(),
            new Rdef03RecordJaxb(),
            legstar.test.jaxb.rdef03.Rdef03Record.class
        );
    }

}
