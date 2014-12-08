package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru03Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Stru03Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.stru03.Stru03Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Stru03RecordJaxb());
        visitor.visit(new CobolStru03Record());
        return new FromHostResult < legstar.test.jaxb.stru03.Stru03Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.stru03.Stru03Record) visitor.getLastObject());
    }    

}
