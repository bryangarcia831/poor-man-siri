/**
  * Bryan Garcia
  * 12/8/16
  */

package expressions

import ui.TypeException
import values._

case class Iteration(iterate: Expression, body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {

    var result: Value = Notification.DONE

    val condition = iterate.execute(env)

    if (condition.isInstanceOf[Boole]) {
      println("inside conditional")
      while (iterate.execute(env).toString() == "true") {
        println("inside while")
        result = body.execute(env)
      }
      println("outside while")
      result
    }
    else throw TypeException("Type Exception error")
  }
}