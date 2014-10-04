import java.io._
import scala.collection.mutable.HashMap

package object profUtils {
  private object stacktraceUtil {
    def indentedPrint(e: Exception) = {
      val sw = new StringWriter
      e.printStackTrace(new PrintWriter(sw))
      val strings = sw.toString.split("\n")
      for (line <- strings) {
        profUtils.indentedPrintln(line)
      }
    }
  }

  private var depth: Int = 0

  private var start = System.nanoTime

  private var currPhase = None

  private val phaseTimeMap = HashMap[Phase, Long]()

  private def msDiff(end: Long, start: Long) = (end - start) / 1000000

  def enterPhase(phase: Phase) = {
    currPhase match {
      case Some(_) =>
        throw new IllegalStateException("Cannot enter phase while.. in a phase!")
      case None => {
        currPhase = Some(phase)
        phaseTimeMap += currPhase -> System.nanoTime
        log(s"Entering phase '${phase.description}'")
      }
    }
  }

  def exitPhase = {
    currPhase match {
      case Some(_) => {
        log(s"Exiting phase '${phase.description}' after ${msDiff(System.nanoTime, phaseTimeMap.get(currPhase))}ms")
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
    indentedPrintln(s"[ ${time}ms ]: $message")
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
    indentedPrintln(label)
    time(f)
  }

  def time[A](f: => A): A = withIndent {
    val start = System.nanoTime()
    val res = f
    val end = System.nanoTime()
    indentedPrintln(s"Time taken: ${(end - start)/1000000}ms")
    res
  }

  private[profUtils] def indentedPrint(s: String) = print(indent + s)
  private[profUtils] def indentedPrintln(s: String) = println(indent + s)
}
