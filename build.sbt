name := "scalajs-java"

version := "1.0"

scalaVersion := "2.11.8"

// adding the tools.jar to the unmanaged-jars seq
unmanagedJars in Compile ~= {uj =>
  Seq(Attributed.blank(file(System.getProperty("java.home").dropRight(3)+"lib/tools.jar"))) ++ uj
}
