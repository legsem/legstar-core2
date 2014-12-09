package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Stru04Record extends Cob2Jaxb<legstar.test.jaxb.stru04.Stru04Record> {

    public Cob2Stru04Record() {
        super(
            new CobolStru04Record(),
            new Stru04RecordJaxb(),
            legstar.test.jaxb.stru04.Stru04Record.class
        );
    }

}
