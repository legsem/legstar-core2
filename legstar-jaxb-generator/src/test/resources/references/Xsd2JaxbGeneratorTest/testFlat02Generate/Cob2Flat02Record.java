package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Flat02Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Flat02Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.flat02.Flat02Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Flat02RecordJaxb());
        visitor.visit(new CobolFlat02Record());
        return new FromHostResult < legstar.test.jaxb.flat02.Flat02Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.flat02.Flat02Record) visitor.getLastObject());
    }    

}
