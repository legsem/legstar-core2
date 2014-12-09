package legstar.samples.custdat;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.legstar.base.FromHostResult;

public class CustdatSampleTest {

    @Test
    public void testConvert() {

        try {
            Cob2CustomerData converter = new Cob2CustomerData();
            byte[] hostData = new byte[converter.getMaxBytesLen()];
            readFully(hostData);
            FromHostResult < CustomerData > result = converter.convert(
                    hostData, 0);
            print(result);

        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private void print(FromHostResult < CustomerData > result)
            throws JAXBException {
        System.out.println("Host bytes converted : "
                + result.getBytesProcessed());
        JAXBContext jaxbContext = JAXBContext.newInstance(CustomerData.class);
        jaxbContext.createMarshaller().marshal(
                new ObjectFactory().createCustomerData(result.getValue()),
                System.out);
    }

    private void readFully(byte[] buffer) throws IOException {
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(new File("src/test/data/custdat.bin"));
            int r = buffer.length;
            while (r > 0) {
                int p = buffer.length - r;
                int c = fis.read(buffer, p, r);
                if (c == -1) {
                    break;
                }
                r -= c;
            }
        } finally {
            if (fis != null) {
                fis.close();
            }
        }

    }

}
