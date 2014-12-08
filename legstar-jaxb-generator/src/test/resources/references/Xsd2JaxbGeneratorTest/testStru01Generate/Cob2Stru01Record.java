package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru01Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Stru01Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.stru01.Stru01Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Stru01RecordJaxb());
        visitor.visit(new CobolStru01Record());
        return new FromHostResult < legstar.test.jaxb.stru01.Stru01Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.stru01.Stru01Record) visitor.getLastObject());
    }    

}
