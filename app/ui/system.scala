/**
  * Bryan Garcia
  */

package ui

import expressions._
import values._

object system {


  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" =>  mul(args)
      case "div" => div(args)
      case "sub" =>sub(args)
      case "equals" => equals(args)
      case "less" =>less(args)
      case "content" => setContent(args)
      case "var" => makeVar(args)

      case _ => throw UndefinedException(opcode.name)
    }
  }

  private def setContent(args: List[Value]): Value = {
    if(args.isEmpty) {
      throw TypeException("error")
    }
    else if(args.head.isInstanceOf[Variable]) {
      args.head.asInstanceOf[Variable].content
    }
    else throw TypeException("")
  }

  private def makeVar(args: List[Value]) = {
    if(args.isEmpty) {
      throw TypeException("error")
    }
    else{
      Variable(args.head)
    }
  }


  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) {
      throw TypeException("addition expects > 0 inputs")
    }
    val ok = vals.filter(_.isInstanceOf[Number])

    if (ok.length < vals.length) {
      throw TypeException("all addition inputs must be numbers")
    }
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ + _)
  }

  private def sub(vals: List[Value]): Value = {

    //Check for errors
    if (vals.isEmpty) throw TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw TypeException("all subtraction inputs must be numbers")

    val args2 = vals.map(_.asInstanceOf[Number])
    var ret = args2.head

    for (temp <- args2) {
      ret = ret - temp
    }

    ret
  }

  private def div(vals: List[Value]): Value = {
    if (checkTwoInputs(vals, "division")) {
      val args2 = vals.map(_.asInstanceOf[Number])

      args2.head / args2.tail.head
    }
    else {
      null
    }
  }

  private def checkTwoInputs(vals: List[Value], testType: String): Boolean = {
    if (vals.isEmpty) throw new TypeException(testType + " than expects > 0 inputs")
    else if (vals.length != 2) throw TypeException(testType + " only can have two inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException(testType + " inputs must only be numbers")

    true
  }

  private def mul(vals: List[Value]): Value = {

    //All checks for errors
    if (vals.isEmpty) throw new TypeException("multiply expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiply inputs must be numbers")

    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ * _)
  }

  private def more(vals: List[Value]): Value = {

    if (checkTwoInputs(vals, "more than")) {
      val args2 = vals.map(_.asInstanceOf[Number])

      args2.head > args2.tail.head
    }
    else {
      null
    }

  }

  private def less(vals: List[Value]): Value = {

    if (checkTwoInputs(vals, "less than")) {
      val args2 = vals.map(_.asInstanceOf[Number])

      args2.head > args2.tail.head
    }
    else {
      null
    }
  }

  private def unequal(vals: List[Value]): Value = {

    if (checkTwoInputs(vals, "unequal")) {
      val args2 = vals.map(_.asInstanceOf[Number])

      args2.head != args2.tail.head
    }
    else {
      null
    }
  }

  private def equals(vals: List[Value]): Value = {

    if (checkTwoInputs(vals, "less than")) {
      val args2 = vals.map(_.asInstanceOf[Number])

      args2.head == args2.tail.head
    }

    else {
      null
    }
  }

}

case class TypeException(s: String) extends Exception {

}

case class UndefinedException(name: String) extends Exception {
  override def toString: String = {
    super.toString + ": " + name
  }
}
