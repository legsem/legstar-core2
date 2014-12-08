package flat01;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Flat01Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Flat01Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < flat01.Flat01Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Flat01RecordJaxb());
        visitor.visit(new CobolFlat01Record());
        return new FromHostResult < flat01.Flat01Record >(
                visitor.getLastPos(),
                (flat01.Flat01Record) visitor.getLastObject());
    }    

}
