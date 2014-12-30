package test.example;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Cflt01Record extends Cob2Jaxb<test.example.Cflt01Record> {

    public Cob2Cflt01Record() {
        super(
            new CobolCflt01Record(),
            new Cflt01RecordJaxb(),
            test.example.Cflt01Record.class
        );
    }

}
