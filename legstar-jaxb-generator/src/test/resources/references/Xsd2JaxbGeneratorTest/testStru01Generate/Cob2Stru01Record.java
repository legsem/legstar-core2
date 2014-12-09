package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Stru01Record extends Cob2Jaxb<legstar.test.jaxb.stru01.Stru01Record> {

    public Cob2Stru01Record() {
        super(
            new CobolStru01Record(),
            new Stru01RecordJaxb(),
            legstar.test.jaxb.stru01.Stru01Record.class
        );
    }

}
