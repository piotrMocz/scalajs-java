# scalajs-java
Project aims to compile Java sources into Scala.js IR

[![Build Status](https://travis-ci.org/piotrMocz/scalajs-java.svg?branch=master)](https://travis-ci.org/piotrMocz/scalajs-java)

### Setup and running
To compile Java programs and transform the trees you only need scala and sbt. To run the programs you also need Node.js. Apart from that, all you need to do is run (in project root):

    > sbt test 

to run all the tests. Please see the `testproject` directory in the project root for an example of how to compile a whole project. Right now the path to the project is defined by the `testProjectPath` in the `Config.scala` file. After you set it to your project path (or keep its original value to use the project provided by the repo), run:
    
    > sbt run
     
Choosing the `Main` class to run (sbt sees our test Java project as a runnable source as well, so you need to specify which one to run).

### Project status
Right now we can compile:
* simple classes
* fields (declaration and access)
* static fields with initializers
* local variables
* primitive types + `String` type
* method declarations
* for loops
* while and do-while loops
* assignments (returning a value)
* assign-ops (e.g. `x += 10`)
* incrementation, decrementation (both pre- and post-)
* `println` method invocation (useful for debugging and testing)
* object creation with `new`
* method invocation in general (although `println` is the only supported library method right now)
* static method invocation
* using multiple files in a project/compiling projects (this has limitations for now)
* generic classes (e.g. `class Test<T> { /* ... */ }`)
