/**
  * Bryan Garcia
  * 11/29/16
  */

package expressions

import values._

case class Block(locals: List[Expression]) extends SpecialForm {

  def execute(env: Environment): Value = {
    val l = locals.length - 1

    val temp = new Environment(env)

    for (i <- 0 until locals.length - 1) {
      locals(i).execute(temp)
    }

    locals(l).execute(temp)
  }

}