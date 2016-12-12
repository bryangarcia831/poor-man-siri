package expressions

/**
  * Created by Bryan Garcia on 11/19/16.
  */

import values._

case class Declaration(id: Identifier, expression: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    env.put(id, expression.execute(env))

    Notification.OK
  }

}