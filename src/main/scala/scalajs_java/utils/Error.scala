package scalajs_java.utils

import scala.collection.mutable.{MutableList => MList}

import scalajs_java.trees.Position

/* Errors thrown by the compiler
 * We would like to fail as late as possible, giving
 * the user as many errors as we can spot.
 * Thus, instead of throwing an exception the moment we
 * see an error, we append it to an error list and
 * fail later.
 */

sealed trait Severity

case object Warning extends Severity {
  override def toString: String = "WARNING"
}

case object Normal extends Severity {
  override def toString: String = "ERROR"
}

case object Fatal extends Severity {
  override def toString: String = "ERROR (fatal)"
}

case class CompilerPhase(stage: String) {
  override def toString: String = stage + " phase"
}

trait Error {
  val line: Int
  val methodName: Option[String]
  val message: String
  val severity: Severity

  def formatError(phase: CompilerPhase): String
}

class CompilerError(val line: Int,
                    val methodName: Option[String],
                    val message: String,
                    val severity: Severity) extends Error {

  override def formatError(phase: CompilerPhase): String = {
    val mName = methodName.map("in method " + _)
    s"""
       | ${severity.toString} during ${phase.toString} ${methodName.getOrElse("")}
       | (line: $line)
       | $message
     """.stripMargin
  }
}

object CompilerError {
  def apply(line: Int, methodName: Option[String],
            message: String, severity: Severity): CompilerError = {
    new CompilerError(line, methodName, message, severity)
  }
}

class ErrorHandler(private val phase: CompilerPhase) {

  private val errors = MList.empty[Error]

  private def reportAndFail(): Unit = {
    val errorMsg = errors.map(_.formatError(phase)).mkString(
      "Errors encountered during compilation:\n\n",
      "\n\n",
      "Terminating.")

    println(errorMsg)
    System.exit(1)
  }

  def fail(line: Int, methodName: Option[String], message: String,
           severity: Severity): Unit =
    this.fail(CompilerError(line, methodName, message, severity))

  def fail(error: Error): Unit = {
    errors += error
    error.severity match {
      case Fatal => reportAndFail()
      case _     => ()
    }
  }

  def reportErrors(): Unit = {
    if (errors.nonEmpty) reportAndFail()
  }

}
