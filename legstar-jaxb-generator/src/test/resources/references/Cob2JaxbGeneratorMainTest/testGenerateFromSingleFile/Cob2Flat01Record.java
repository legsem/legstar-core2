package flat01;

import com.legstar.jaxb.converter.Cob2Jaxb;

public class Cob2Flat01Record extends Cob2Jaxb<flat01.Flat01Record> {

    public Cob2Flat01Record() {
        super(
            new CobolFlat01Record(),
            new Flat01RecordJaxb(),
            flat01.Flat01Record.class
        );
    }

}
