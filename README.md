# scalajs-java
Project aims to compile Java sources into Scala.js IR

[![Build Status](http://141.5.102.128:8080/job/scalajs_java/badge/icon)](http://141.5.102.128:8080/job/scalajs_java/)

### Setup and running
To compile Java programs and transform the trees you only need scala and sbt. To run the programs you also need Node.js. Apart from that, all you need to do is run (in project root):

    > sbt test 

to run all the tests. To compile a Java file, modify the file `Test.java` (in the project root). The file will be compiled and the ouput printed (along with a lot of diagnostic/debug info) after running:

    > sbt run

You can change the path to the file that gets compiled; for now it is hard-coded in the `scalajs_java.Config` file but that will be subject to change as the compiler gets more sophisticated.

### Project status
Right now we can compile:
* simple classes
* fields
* local variables
* primitive types + `String` type
* method declarations
* for loops
* while loops
* assignments (returning a value)
* assign-ops (e.g. `x += 10`)
* incrementation, decrementation (both pre- and post-)
* `println` method invocation (useful for debugging and testing)
