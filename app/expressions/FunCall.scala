/**
  * Bryan Garcia
  */
package expressions

import ui._
import values._

case class FunCall(op: Expression, args: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val static: Boolean = false

    var env2 = env
    if (!static) env2 = null
    val temp = args.map(_.execute(env))

    try {
      if (op.execute(env).isInstanceOf[Closure]) {
        op.execute(env).asInstanceOf[Closure].apply(temp, env2)
      }
      else {
        throw UndefinedException(env.toString())
      }
    } catch {
      case e: UndefinedException => system.execute(op.asInstanceOf[Identifier], temp)
    }

  }


}