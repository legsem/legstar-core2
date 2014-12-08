package test.example;

import com.legstar.base.context.CobolContext;
import com.legstar.base.context.EbcdicCobolContext;
import com.legstar.base.type.FromHostResult;
import com.legstar.jaxb.converter.Cob2JaxbConverter;

public class Cob2CustomerData {
    
    private final CobolContext cobolContext;
    
    public Cob2CustomerData() {
        cobolContext = new EbcdicCobolContext();
    }
    
    public FromHostResult < legstar.test.jaxb.cusdat.CustomerData > convert(
            byte[] hostData, int start) {
        Cob2JaxbConverter visitor = new Cob2JaxbConverter(cobolContext,
                hostData, start, new CustomerDataJaxb());
        visitor.visit(new CobolCustomerData());
        return new FromHostResult < legstar.test.jaxb.cusdat.CustomerData >(
                visitor.getLastPos(),
                (legstar.test.jaxb.cusdat.CustomerData) visitor.getLastObject());
    }    

}
