/**
  * Created by Bryan Garcia on 11/19/16.
  */

package expressions

import ui._
import values._

case class Conjunction(cond1: Expression, cond2: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    val executedCondition1 = cond1.execute(env)

    if (executedCondition1.isInstanceOf[Boole]) {

      if (!executedCondition1.asInstanceOf[Boole].value) {
        return Boole(executedCondition1.asInstanceOf[Boole].value)
      }

      val executedCondition2 = cond2.execute(env)
      if (executedCondition1.isInstanceOf[Boole]) {
        return Boole(executedCondition1.asInstanceOf[Boole].value && executedCondition2.asInstanceOf[Boole].value)
      }

    }

    throw TypeException("All must be booles")
  }
}