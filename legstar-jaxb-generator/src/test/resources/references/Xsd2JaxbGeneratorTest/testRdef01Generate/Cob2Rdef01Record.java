package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef01Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Rdef01Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.rdef01.Rdef01Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Rdef01RecordJaxb());
        visitor.visit(new CobolRdef01Record());
        return new FromHostResult < legstar.test.jaxb.rdef01.Rdef01Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.rdef01.Rdef01Record) visitor.getLastObject());
    }    

}
