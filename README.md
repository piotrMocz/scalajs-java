# scalajs-java

Project aims to compile Java sources into Scala.js IR

**IMPORTANT**
This project is in a _very_ early phase and is not ready to use with the rest of the Scala.js ecosystem. 

[![Build Status](https://travis-ci.org/piotrMocz/scalajs-java.svg?branch=master)](https://travis-ci.org/piotrMocz/scalajs-java)

### Setup and running

To compile Java programs and transform the trees you only need scala and sbt. To run the programs you also need Node.js. Apart from that, all you need to do is run (in project root):

    > sbt test 

to run all the tests. Please see the `testproject` directory in the project root for an example of how to compile a whole project. Right now the path to the project is defined by the `testProjectPath` in the `Config.scala` file. After you set it to your project path (or keep its original value to use the project provided by the repo), run:
    
    > sbt run
     
Choosing the `Main` class to run (sbt sees our test Java project as a runnable source as well, so you need to specify which one to run).

### Project structure

In the `java` directory you can find a thin layer of Javac interface. `CompilerInterface` is the main object of interest there, rest of the files are just utility classes in one way or another.

The `scala` directory contains the compiler itself and is structured as follows:
 
     src/main/scala
     └── scalajs_java
         ├── compiler
         │   ├── Adapt.scala
         │   ├── CompilerPipeline.scala
         │   ├── Compiler.scala
         │   ├── Definitions.scala
         │   ├── OpCompiler.scala
         │   ├── passes
         │   │   ├── AdaptPass.scala
         │   │   ├── CompilerPass.scala
         │   │   ├── ConstructorPass.scala
         │   │   ├── EnclClassPass.scala
         │   │   ├── ErasurePass.scala
         │   │   ├── ExpSymsPass.scala
         │   │   ├── JTraversePass.scala
         │   │   ├── OpTraversePass.scala
         │   │   ├── Pass.scala
         │   │   ├── RefTagPass.scala
         │   │   └── StaticInitsPass.scala
         │   ├── TypeCompiler.scala
         │   └── Utils.scala
         ├── Config.scala
         ├── Linker.scala
         ├── Main.scala
         ├── Runner.scala
         ├── traversals
         │   ├── ConstructorsTraverse.scala
         │   ├── EnclClassTraverse.scala
         │   ├── ErasureTraverse.scala
         │   ├── ExportedSymbolsTraverse.scala
         │   ├── JTreeTraverse.scala
         │   ├── OperationsTraverse.scala
         │   ├── RefTraverse.scala
         │   ├── StaticInitsTraverse.scala
         │   └── Traverse.scala
         ├── trees
         │   ├── Tree.scala
         │   └── Type.scala
         └── utils
             ├── ClassScope.scala
             ├── Error.scala
             ├── Mangler.scala
             ├── Predicates.scala
             └── Scope.scala

The `scalajs_java.compiler` package is home to some of the key functionality of the compiler:

* `Adapt` performs (parts of) the type erasure. This module will be changing a lot in the future
* `Compiler` transforms Java AST into Scala.js IR
* `CompilerPipeline` is responsible for pushing the AST through all the compilation passes and collecting the results
* `Definitions` contains pre-defined pieces of Scala.js IR for common constructs like the `println` method
* `OpCompiler` is responsible for choosing the right type of operation given its operands
* `scalajs_java.compiler.passes` directory contains the passes/stages of the compilation. Each pass takes AST as input and produces AST as output (with possible side-effects) and is in essence a generic wrapper around the tree traversal
* `TypeCompiler` transforms Java types into their Scala.js IR counterparts
* `Utils` is, as the name suggests, a set of utility methods
* `Config` contains hard-coded configuration (paths)
* `Linker` is the linker module borrowed from @sjrd's toy `scalajsir-calc` project
* `Main` is the compiler's entry point, launching the whole pipeline
* `Runner` is, just as linker, borrowed from @sjrd
* `scalajs_java.traversals` contains the classes transforming/traversing the AST to either enrich it, desugar it or collect some information like exposed symbols. They may be combined at a later stage to increase performance. Right now they are separated to increase the modularity
* `scalajs_java.trees.Trees` module contains the Javac's AST rewritten in Scala to facilitate easier processing
* `scalajs_java.trees.Types` module contains, above all, a Scala wrapper around the Javac's type representation and some additional types. This needs to be changed (and will be changed) in the near future. Every type needs to have its own Scala representation, because instantiating Javac's types is very cumbersome and forces us to do ugly workarounds
* `scalajs_java.utils` contains utility classes shared by different compiler parts (unlike the `scalajs_compiler.compiler.Utils` class which is used exclusively by the `scalajs_compiler.compiler` package).

### Project status

Right now we can compile (or include in our feature set):

* simple classes
* interfaces
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
* creating arrays
* accessing/updating arrays
* using multiple files in a project/compiling projects (this has limitations for now)
* generic classes (e.g. `class Test<T> { /* ... */ }`)
* `extends` and `implements` mechanisms
* reporting errors from Javac (both parsing and typechecking)

### TODO

* refactoring of types -- we need to create our own type representation instead of re-using the one from Javac. This is because Javac's types contain a lot of information that we don't need and creating them correctly is complicated (if not impossible outside of Javac's internals). We need to create (or at least modify) the types during our erasure passes. In general, the compilers structure will be much better once we switch to our own type representation
* compile lambdas
* improve generics and type erasure (type bounds, unions, wildcards, etc.)
* ...
