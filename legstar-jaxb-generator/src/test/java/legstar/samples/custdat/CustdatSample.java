package legstar.samples.custdat;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.legstar.base.converter.FromHostResult;


/**
 * Sample code that invokes a converter to transform host data read off a file
 * into a JAXB instance.
 * <p>
 * This sample uses the {@link Cob2CustomerDataConverter} class that was generated by the
 * legstar JAXB generator starting from the CUSTDAT COBOL copybook.
 * <p>
 * In this sample the host data is read from a file for convenience but it could
 * come from any source (RPC, JMS, ...).
 *
 */
public class CustdatSample {

    public static void main(final String[] args) {
        CustdatSample main = new CustdatSample();
        main.convert(args[0]);
    }

    public void convert(String filePath) {

        // Create a reusable, thread safe, converter.
        Cob2CustomerDataConverter converter = new Cob2CustomerDataConverter();

        // Get mainframe data (file, RPC, JMS, ...)
        byte[] hostData = readSampleData(filePath);

        // Convert the mainframe data to JAXB
        FromHostResult < CustomerData > result = converter.convert(
                hostData);

        // Print out the results
        print(result);

    }

    /**
     * Printout the result of the conversion.
     * <p>
     * The JAXB instance produced is serialized as XML for readability.
     * 
     * @param result the result of the legstar conversion to a JAXB instance
     */
    private void print(FromHostResult < CustomerData > result) {
        try {
            System.out.println("Host bytes converted : "
                    + result.getBytesProcessed());
            System.out.println("Result JAXB instance as XML :");
            JAXBContext jaxbContext = JAXBContext.newInstance(CustomerData.class);
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            marshaller.marshal(
                    new ObjectFactory().createCustomerData(result.getValue()),
                    System.out);
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Sample mainframe data for a single record come from a file.
     * <p>
     * In a real situation there would be more than one record in the file but
     * we keep things simple here.
     * 
     * @return a buffer full of mainframe data
     */
    private byte[] readSampleData(String filePath) {
        File sampleFile = new File(filePath);
        byte[] buffer = new byte[(int) sampleFile.length()];
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(sampleFile);
            readFill(fis, buffer);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                fis.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return buffer;
    }

    /**
     * Fill a buffer with raw bytes read off a stream.
     * 
     * @param stream the input stream
     * @param buffer to fill with bytes to read off the stream
     * @return bytes read (might be smaller than buffer length at EOF)
     * @throws IOException if unable to read the stream
     */
    private int readFill(InputStream stream, byte[] buffer)
            throws IOException {
        int r = buffer.length;
        while (r > 0) {
            int p = buffer.length - r;
            int c = stream.read(buffer, p, r);
            if (c == -1) {
                break;
            }
            r -= c;
        }
        return buffer.length - r;

    }

}
