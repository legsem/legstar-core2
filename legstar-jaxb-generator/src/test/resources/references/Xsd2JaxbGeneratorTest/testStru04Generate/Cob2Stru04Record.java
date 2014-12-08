package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Stru04Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Stru04Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.stru04.Stru04Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Stru04RecordJaxb());
        visitor.visit(new CobolStru04Record());
        return new FromHostResult < legstar.test.jaxb.stru04.Stru04Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.stru04.Stru04Record) visitor.getLastObject());
    }    

}
