package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef02Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Rdef02Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.rdef02.Rdef02Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Rdef02RecordJaxb());
        visitor.visit(new CobolRdef02Record());
        return new FromHostResult < legstar.test.jaxb.rdef02.Rdef02Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.rdef02.Rdef02Record) visitor.getLastObject());
    }    

}
