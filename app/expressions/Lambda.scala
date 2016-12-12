/**
  * Bryan Garcia
  * 11/29/16
  */

package expressions

import values._

case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    Closure(params, body, env)
  }

}