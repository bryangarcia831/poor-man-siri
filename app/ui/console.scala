/**
  * Bryan Garcia
  */

package ui

import values._
import utilties.parsers._

object console {
  val parsers = new Parsers
  val globalEnv = Environment()

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)

    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => "" + tree.get.execute(globalEnv)
    }
  }

  def repl {
    // declare locals
    var more = true
    while(more) {
      try {
        // read/execute/print
        print("-> ")
        var cmd = readLine
        if (cmd=="quit") {
          println("Bye")
          more = false
        }
        else
        {
          println(execute(cmd).toString())
        }

      }
      catch {
        case e: SyntaxException => {
          println(e.toString())
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: UndefinedException => {
          println(e.toString)
        }

      } finally {
        Console.flush
      }
    }
  }

  def main(args: Array[String]): Unit = { repl }

}
