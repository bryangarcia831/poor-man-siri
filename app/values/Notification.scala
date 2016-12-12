package values

/**
  * Bryan Garcia
  */

class Notification(message: String) extends Value {
  override def toString(): String = {
    return message
  }
}

object Notification {
  val OK = Notification("OK")
  val DONE = Notification("DONE")
  val UNSPECIFIED = Notification("UNSPECIFIED")
  val BROKEN = Notification("BROKEN CODE")

  def apply(msg: String) = new Notification(msg)

}