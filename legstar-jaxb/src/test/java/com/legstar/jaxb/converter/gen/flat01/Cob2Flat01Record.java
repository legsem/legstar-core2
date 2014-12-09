package com.legstar.jaxb.converter.gen.flat01;

import com.legstar.base.FromHostResult;
import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.jaxb.converter.Cob2JaxbVisitor;

public class Cob2Flat01Record {
    
    private final CobolContext cobolContext;
    
    public Cob2Flat01Record() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.flat01.Flat01Record > convert(
            byte[] hostData, int start) {
        Cob2JaxbVisitor visitor = new Cob2JaxbVisitor(cobolContext,
                hostData, start, new Flat01RecordJaxb());
        visitor.visit(new legstar.test.jaxb.flat01.CobolFlat01Record());
        return new FromHostResult < legstar.test.jaxb.flat01.Flat01Record >(
                visitor.getLastPos(),
                (legstar.test.jaxb.flat01.Flat01Record) visitor.getLastObject());
    }    

}
