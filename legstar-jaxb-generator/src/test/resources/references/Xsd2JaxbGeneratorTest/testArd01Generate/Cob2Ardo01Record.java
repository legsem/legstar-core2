package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2Ardo01Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Ardo01Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.ardo01.Ardo01Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new Ardo01RecordJaxb());
        visitor.visit(new CobolArdo01Record());
        return new FromHostResult < legstar.test.jaxb.ardo01.Ardo01Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.ardo01.Ardo01Record) visitor.getLastObject());
    }    

}
