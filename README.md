legstar core V2
===============

A COBOL to Java conversion toolkit.

## Objectives

* Provide conversion capabilities for data described by [COBOL copybooks](https://en.wikipedia.org/wiki/COBOL#Data_division)

## Restrictions

* Recognizes the [IBM COBOL for z/OS](https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library) syntax only. It may or may not work for other COBOL compilers.

## Requirements

* Java JDK 8 and above (It is important that this is a [JDK](https://en.wikipedia.org/wiki/Java_Development_Kit) not a simple JRE)

* Maven 3  (If you build from sources)

* Ant 1.9.x (To run samples)

## Build from sources

1. Clone the [GIT repository](https://github.com/legsem/legstar-core2.git)

2. From a command window, while located in the folder where you cloned the repo, type:

>   `mvn clean install`

## Run the base sample

  If you built the project from sources, you will find the distribution *zip* file under *legstar-core2/legstar-base-generator/target*.

  Otherwise you can get the latest released zip [here](https://central.sonatype.com/artifact/com.legsem.legstar/legstar-base-generator).

  Unzip the zip file in a location of your choice.

  Go to the *samples* folder and type:

>   `ant`

## Run the JAXB sample

  If you built the project from sources, you will find the distribution *zip* file under *legstar-core2/legstar-jaxb-generator/target*.

  Otherwise you can get the latest released zip [here](https://central.sonatype.com/artifact/com.legsem.legstar/legstar-jaxb-generator).

  Unzip the zip file in a location of your choice.

  Go to the *samples* folder and type:

>   `ant`

## Using the legstar-core2 API:

There are 2 data converters at the moment:

* **com.legstar.base.converter.Cob2HashMapConverter** converts Cobol data to a Java HashMap where keys correspond to Cobol fields
* **com.legstar.jaxb.converter.Cob2JaxbConverter<J>** converts Cobol data to a Java JAXB instance where properties correspond to Cobol fields

Before you can use one of these converters though you have to translate a  COBOL copybook into a usable model.

Cobol copybooks are like metadata describing a data structure. This is an example of a Cobol copybook:

       01  CUSTOMER-DATA.
           05 CUSTOMER-ID                    PIC 9(6).
           05 PERSONAL-DATA.
              10 CUSTOMER-NAME               PIC X(20).
              10 CUSTOMER-ADDRESS            PIC X(20).
              10 CUSTOMER-PHONE              PIC X(8).
           05 TRANSACTIONS.
              10 TRANSACTION-NBR             PIC 9(9) COMP.
              10 TRANSACTION OCCURS 0 TO 5
                 DEPENDING ON TRANSACTION-NBR.
                 15 TRANSACTION-DATE         PIC X(8).
                 15 FILLER REDEFINES TRANSACTION-DATE.
                    20 TRANSACTION-DAY       PIC X(2).
                    20 FILLER                PIC X.
                    20 TRANSACTION-MONTH     PIC X(2).
                    20 FILLER                PIC X.
                    20 TRANSACTION-YEAR      PIC X(2).
                 15 TRANSACTION-AMOUNT       PIC S9(13)V99 COMP-3.
                 15 TRANSACTION-COMMENT      PIC X(9).
                 
The Cobol data itself is binary. To visualize such binary data you can use [HxD](https://mh-nexus.de/en/hxd/) a free editor that supports Cobol data.

This is an example of data corresponding to CUSTOMER-DATA:

![Hxd editor with customer data](https://github.com/legsem/legstar-core2/blob/master/custdat.bin.png)

### Using Cob2HashMapConverter

You convert Cobol binary data to Java using code like this:

```
byte[] cobolData = Files.readAllBytes(Paths.get("src/test/data/custdat.bin"));
Cob2HashMapConverter converter = new Cob2HashMapConverter.Builder()
        .cobolComplexType(new legstar.samples.custdat.CobolCustomerData())
        .build();
FromHostResult<Map<String, Object>> res = converter.convert(cobolData);
System.out.println(res.getBytesProcessed());
System.out.println(res.getValue());
```

The output should look something like this:

```
158
{customerId=2, personalData={customerName=FRED BROWN, customerAddress=CAMBRIDGE, customerPhone=38791206}, transactions={transactionNbr=4, transaction=[{transactionDateChoice={transactionDate=30/10/10}, transactionAmount=36.82, transactionComment=*********}, {transactionDateChoice={transactionDate=30/10/10}, transactionAmount=175.93, transactionComment=*********}, {transactionDateChoice={transactionDate=30/10/10}, transactionAmount=114.92, transactionComment=*********}, {transactionDateChoice={transactionDate=10/04/11}, transactionAmount=229.65, transactionComment=********}]}}
```

At this stage you may be wondering where the *legstar.samples.custdat.CobolCustomerData* java class came from.

CobolCustomerData is a java class that represents the Cobol fields in the CUSTOMER-DATA copybook.

CobolCustomerData is generated using code like this:

```
String copybook = new String(Files.readAllBytes(Paths.get("src/test/cobol/CUSTDAT.cpy")));
Cob2CobolTypesGenerator generator = new Cob2CobolTypesGenerator();
Map<String, String> sources = generator.generate(copybook, "legstar.samples.custdat");
System.out.println(sources.get("CobolCustomerData"));
```

Once generated, you store CobolCustomerData in the appropriate location and compile it. Its only dependency is on the [legstar-base](https://github.com/legsem/legstar-core2/tree/master/legstar-base) module.

### Using Cob2JaxbConverter

Similar to *Cob2HashMapConverter*, this is a 2 stage process.

You first need to generate java support classes from the Cobol copybook.

Here is how you would do it in the JAXB case:

```
String copybook = new String(Files.readAllBytes(Paths.get("src/test/cobol/CUSTDAT.cpy")));
Cob2JaxbGenerator generator = new Cob2JaxbGenerator();
generator.generate(copybook, Paths.get("target/generated-sources"), "legstar.samples.custdat");
```

If you check the target/generated-sources folder, you should see a bunch of java classes. 

One of the generated classes is the same CobolCustomerData we saw for Cob2HashMapConverter.

There is also a set of annotated JAXB classes (CustomerData, PersonalData, ...).

Finally, there is a specialized converter called Cob2CustomerDataConverter.

Here is how you could use the converter:

```
byte[] cobolData = Files.readAllBytes(Paths.get("src/test/data/custdat.bin"));
Cob2CustomerDataConverter converter = new Cob2CustomerDataConverter();
FromHostResult<CustomerData> res = converter.convert(cobolData);
JAXBContext context = JAXBContext.newInstance(CustomerData.class);
Marshaller marshaller = context.createMarshaller();
marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
marshaller.marshal(
        new ObjectFactory().createCustomerData(res.getValue()),
        System.out);
```

This should produce output such as:

```
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<customerData xmlns="http://samples.legstar/custdat">
    <customerId>2</customerId>
    <personalData>
        <customerName>FRED BROWN</customerName>
        <customerAddress>CAMBRIDGE</customerAddress>
        <customerPhone>38791206</customerPhone>
    </personalData>
    <transactions>
        <transactionNbr>4</transactionNbr>
        <transaction>
            <transactionDate>30/10/10</transactionDate>
            <transactionAmount>36.82</transactionAmount>
            <transactionComment>*********</transactionComment>
        </transaction>
        <transaction>
            <transactionDate>30/10/10</transactionDate>
            <transactionAmount>175.93</transactionAmount>
            <transactionComment>*********</transactionComment>
        </transaction>
        <transaction>
            <transactionDate>30/10/10</transactionDate>
            <transactionAmount>114.92</transactionAmount>
            <transactionComment>*********</transactionComment>
        </transaction>
        <transaction>
            <transactionDate>10/04/11</transactionDate>
            <transactionAmount>229.65</transactionAmount>
            <transactionComment>********</transactionComment>
        </transaction>
    </transactions>
</customerData>
```

