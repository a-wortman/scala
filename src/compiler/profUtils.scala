import java.io._
import scala.collection.mutable.HashMap
import scala.reflect.internal.Phase

package object profUtils {
  private object stacktraceUtil {
    def indentedPrint(e: Exception): Unit = {
      val sw = new StringWriter
      e.printStackTrace(new PrintWriter(sw))
      val strings = sw.toString.split("\n")
      val trimmed = strings
        .takeWhile(_.indexOf("xsbt.CachedCompiler") == -1)
        .filter(_.indexOf("at profUtils") == -1)
        .mkString("\n")
      profUtils.indentedPrintln(trimmed)
    }
  }

  private var depth: Int = 0

  private var start = System.nanoTime

  private var currPhase: Option[Phase] = None

  private val phaseTimeMap = HashMap[Phase, Long]()

  private def msDiff(end: Long, start: Long) = (end - start) / 1000000

  def enterPhase(phase: Phase) = {
    currPhase match {
      case Some(_) =>
        throw new IllegalStateException("Cannot enter phase while.. in a phase!")
      case None => {
        currPhase = Some(phase)
        phaseTimeMap += phase -> System.nanoTime
        log(s"Entering phase '${phase.description}'")
      }
    }
  }

  def exitPhase = {
    currPhase match {
      case Some(phase) => {
        log(s"Exiting phase '${phase.description}' after ${msDiff(System.nanoTime, phaseTimeMap(phase))}ms")
        currPhase = None
      }
      case None => {
        throw new IllegalStateException("Cannot exit phase when not in a phase!")
      }
    }
  }

  def restartTiming: Unit = start = System.nanoTime

  def log(message: String): Unit = {
    val time = msDiff(System.nanoTime, start)
    indentedPrintln(s"[ ${time}ms ]: ${message}")
  }

  def stacktrace: Unit = stacktrace("Execution trace: ---! No real error !---")
  def stacktrace(traceMsg: String): Unit = {
    try {
      throw new RuntimeException(traceMsg)
    } catch {
      case e: Exception => stacktraceUtil.indentedPrint(e)
    }
  }

  private def indent = "| " * depth

  private def withIndent[A](f: => A): A = {
    depth = depth + 1
    val ret = f
    depth = depth - 1
    ret
  }

  def time[A](label: String)(f: => A): A = {
    indentedPrintln(label, ("↱ ", "| "))
    timeTrace(f)
  }

  def time[A](label: String, t: Int)(f: => A): A = {
    indentedPrintln(label, ("↱ ", "| "))
    timeTrace(f, t)
  }

  def time[A](f: => A): A = timeTrace(f)

  private def timeTrace[A](f: => A, threshold: Int = 0): A = {
    case class Result[A](res: A, msTime: Long)

    val result = withIndent {
      val start = System.nanoTime()
      val res = f
      val end = System.nanoTime()
      Result(res, (end - start)/1000000)
    }

    indentedPrintln(s"↳ Time taken: ${result.msTime}ms")

    if (threshold != 0 && result.msTime > threshold) {
      profUtils.stacktrace
    }

    result.res
  }

  private[profUtils] def indentedPrintln(s: String, prefixes: (String, String) = ("", "")): Unit = {
    val lines = s.trim.split("\n")
    val iter = lines.iterator
    if (!iter.hasNext) return
    println(indent + prefixes._1 + iter.next)

    for (line <- iter) {
      println(indent + prefixes._2 + line)
    }
  }
}
