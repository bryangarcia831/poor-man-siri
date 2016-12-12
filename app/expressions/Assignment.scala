package expressions

/**
  * Bryan Garcia
  */

import values._
import ui._

case class Assignment(identity: Identifier, update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {

    val function = identity.execute(env)

    if(function.isInstanceOf[Variable])
    {
      function.asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE
    }

    else throw TypeException("Assignment must be a variable")
  }
}