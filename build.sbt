import java.io.PrintWriter

name := "scalajs-java"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-deprecation", "-feature", "-unchecked", "-encoding", "utf-8")

val scalaJSVersion = "0.6.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-js" %% "scalajs-tools" % scalaJSVersion,
  "org.scala-js" %% "scalajs-js-envs" % scalaJSVersion,
  "com.lihaoyi" %% "fastparse" % "0.3.7",

  "com.novocode" % "junit-interface" % "0.9" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-a")

// Some magic to add the scalajs-library.jar in the resources
ivyConfigurations += config("linkingdeps").hide
libraryDependencies +=
  "org.scala-js" %% "scalajs-library" % scalaJSVersion % "linkingdeps"
resourceGenerators in Compile += Def.task {
  val jars = update.value.select(configurationFilter("linkingdeps"))
  for (jar <- jars) yield {
    val dest = (resourceManaged in Compile).value / "scalajs_java" / "runtime" / jar.getName
    IO.copyFile(jar, dest, preserveLastModified = true)
    dest
  }
}.taskValue

// adding the tools.jar to the unmanaged-jars seq
unmanagedJars in Compile ~= {uj =>
  Seq(Attributed.blank(file(System.getProperty("java.home").dropRight(3)+"lib/tools.jar"))) ++ uj
}
