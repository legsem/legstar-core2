package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Rdef03Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Rdef03Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.rdef03.Rdef03Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Rdef03RecordJaxb());
        visitor.visit(new CobolRdef03Record());
        return new FromHostResult < legstar.test.jaxb.rdef03.Rdef03Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.rdef03.Rdef03Record) visitor.getLastObject());
    }    

}
