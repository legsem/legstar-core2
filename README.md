legstar core V2
===============

A COBOL to Java conversion framework.

A new take on the [original legstar project](https://code.google.com/p/legstar/)

## Objectives

* Provide conversion capabilities for data described by [COBOL copybooks](http://en.wikipedia.org/wiki/Include_directive)

* Provide a faster alternative to legstar for use cases that require massive conversions (Hadoop, ETL, ...)

* Provide a simpler API than legstar, using java generics and explicit thread safety.

## Non objectives

* Backward compatibility with legstar. The API V2 is not compatible.

* Complete functional equivalence to legstar. Rarely used legstar features will not be ported to V2.

* Recognizes the IBM COBOL for z/OS syntax only. It may or may not work for other COBOL compilers.

## Requirements

* Java JDK 6 and above (It is important that this is a [JDK](http://en.wikipedia.org/wiki/Java_Development_Kit) not a simple JRE)

* Maven 3  (If you build from sources)

* Ant 1.9.x (To run samples)

## Build from sources

1. Clone the [GIT repository](https://github.com/legsem/legstar-core2.git)

2. From a command window, while located in the folder where you cloned the repo, type:

>   `mvn clean install`

## Run the JAXB sample

  If you built the project from sources, you will find the distribution *zip* file under *legstar-core2/legstar-jaxb-generator/target*.

  Otherwise you can get the latest released zip [here](http://search.maven.org/#search%7Cga%7C1%7Clegstar-jaxb-generator).

  Unzip the zip file in a location of your choice.

  Go to the *samples* folder and type:

>   `ant`
