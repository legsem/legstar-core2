package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Stru03Record extends Cob2Jaxb<legstar.test.jaxb.stru03.Stru03Record> {

    public Cob2Stru03Record() {
        super(
            new CobolStru03Record(),
            new Stru03RecordJaxb(),
            legstar.test.jaxb.stru03.Stru03Record.class
        );
    }

}
