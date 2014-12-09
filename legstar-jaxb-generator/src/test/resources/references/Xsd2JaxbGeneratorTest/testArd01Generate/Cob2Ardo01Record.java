package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Ardo01Record extends Cob2Jaxb<legstar.test.jaxb.ardo01.Ardo01Record> {

    public Cob2Ardo01Record() {
        super(
            new CobolArdo01Record(),
            new Ardo01RecordJaxb(),
            legstar.test.jaxb.ardo01.Ardo01Record.class
        );
    }

}
