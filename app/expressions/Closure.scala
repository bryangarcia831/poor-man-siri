/**
  * Bryan Garcia
  * 11/29/16
  */

package expressions

import ui._
import values._

case class Closure(params: List[Identifier], body: Expression, env: Environment) extends Value {
  def apply(args: List[Value], env: Environment): Value = {

    var temp = new Environment()
    if (env == null) {
      temp = new Environment(env)
    }
    else {
      temp = env
    }

    if (params.length == args.length) {
      temp.put(params, args)
      body.execute(temp)
    }

    else throw TypeException("")

  }

}