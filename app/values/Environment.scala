/**
  * Bryan Garcia
  */

package values

import expressions._
import scala.collection.mutable.HashMap



case class Environment(nextEnv: Environment = null) extends HashMap[Identifier,Value] with Value{

  def put(names: List[Identifier], vals: List[Value]) {

    for (i <- 0 until Math.min(names.length,vals.length))
    {
      put(names(i), vals(i))
    }


  }
  def find(input: Identifier): Value = {
    if (!this.contains(input)  && nextEnv!=null) {
      nextEnv.find(input)
    }
    else if ( !this.contains(input)  && nextEnv==null) {
      Notification.BROKEN
    }
    else {
      this.get(input).get
    }

  }

}


