package expressions

/**
  * Bryan Garcia
  */

import ui._
import values._

case class Disjunction(cond1: Expression, cond2: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {

    val executedCondition1 = cond1.execute(env)
    if (executedCondition1.isInstanceOf[Boole]) {

      if (executedCondition1.asInstanceOf[Boole].value) {
        return executedCondition1.asInstanceOf[Boole]
      }

      val executedCondition2 = cond2.execute(env)
      if (executedCondition1.isInstanceOf[Boole]) {
        return Boole(executedCondition1.asInstanceOf[Boole].value || executedCondition2.asInstanceOf[Boole].value)
      }
    }

    throw new TypeException("All must be booles")
  }
}