package scala.tools.util

trait Logging {
  private val logging: Boolean = false
  private val debug: Boolean = false

  def logError(msg: String, t: Throwable): Unit = ()

  @inline
  final protected def printResult[T](msg: String)(result: T) = {
    _log(msg + ": " + result)
    result
  }

  @inline
  final protected def _log[T](msg: => AnyRef) = if(logging) println(msg)

  @inline
  final protected def debuglog(msg: => String) = if(logging && debug) _log("Debug: " +msg)

  @inline
  final protected def logResult[T](msg: => String)(result: T): T = {
    _log(msg + ": " + result)
    result
  }

  @inline
  final protected def debuglogResult[T](msg: => String)(result: T): T = {
    debuglog(msg + ": " + result)
    result
  }
}
