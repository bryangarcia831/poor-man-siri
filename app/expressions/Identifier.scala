/**
  * Bryan Garcia
  */

package expressions

import values._

import ui.UndefinedException

case class Identifier(name: String) extends Expression with Serializable {

  def execute( env : Environment): Value = {
    if (env.find(this)==values.Notification.UNSPECIFIED) {
      throw UndefinedException("Undefined Identifier: " + name)
    }
    else {
      env.find(this)
    }
  }
}