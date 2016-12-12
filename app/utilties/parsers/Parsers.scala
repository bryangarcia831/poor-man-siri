/**
  * Bryan Garcia
  */

package ui

import expressions._
import values._

import scala.util.parsing.combinator._


class Parsers extends RegexParsers {

  //LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Literal] = boole | number

  //FUNCALL ::= TERM~OPERANDS?
  def funcall: Parser[Expression] = term ~ opt(operands) ^^ {
    case t ~ None => t
    case t ~ Some(Nil) => FunCall(t, Nil)
    case t ~ Some(ops) => FunCall(t, ops)
  }


  //OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }


  //EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case inequal ~ Nil => inequal
    case inequal1 ~ inequals => FunCall(Identifier("equals"), inequal1 :: inequals)
  }


  //INEQUALITY ::= SUM~((<|>|<=|>=|!=)~SUM)*
  def inequality: Parser[Expression] = sum ~ rep("<" ~> sum) ^^ {
    case inequal ~ Nil => inequal
    case inequal ~ inequalList => FunCall(Identifier("less"), inequal :: inequalList)
  }


  //PRODUCT ::= FUNCALL~((\*|/)~FUNCALL)*
  def product: Parser[Expression] = funcall ~ rep("""\*|/""".r ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => divisor(s) }) ^^ {
    case operand ~ Nil => operand
    case operand1 ~ operand2 => FunCall(Identifier("mul"), operand1 :: operand2)
  }


  def divisor(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1.0)
    FunCall(div, List(one, exp))
  }


  // DECLARATION ::= def~IDENTIFIER~=~EXPRESSION
  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  //SUM ::= PRODUCT~((\+|-)~PRODUCT)*
  def sum: Parser[Expression] =
  product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
    case p ~ Nil => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)
  }


  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0.0)
    FunCall(sub, List(zero, exp))
  }


  //NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def number: Parser[Number] =
  """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
    e => Number(e.toDouble)
  }


  //IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
  def identifier: Parser[Identifier] =
  """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    e => Identifier(e)
  }


  // BOOLE ::= true | false
  def boole: Parser[Boole] =
  """true|false""".r ^^ {
    e => Boole(e.toBoolean)
  }


  //CONJUNCTION ::= EQUALITY~(&&~EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("""&&""".r ~> equality) ^^ {
    case cond ~ Nil => cond
    case cond1 ~ conds2 => Conjunction(cond1, conds2.reduce(Conjunction(_, _)))
  }


  //DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case cond ~ Nil => cond
    case cond1 ~ cond2s => Disjunction(cond1, cond2s.reduce(Disjunction(_, _)))
  }


  //CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ conditions ~ ")" ~ consequent ~ Some("else" ~ antecedent) => Conditional(conditions, consequent, antecedent)
    case "if" ~ "(" ~ conditions ~ ")" ~ consequent ~ None => Conditional(conditions, consequent)
  }


  //PARAMETERS ::= ((IDENTIFIER (, IDENTIFIER)*))?)
  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }


  ///LAMBDA ::= lambda PARAMETERS EXPRESSION
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }


  //BLOCK ::- {EXPRESSION (; EXPRESSION)*}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ explist ~ "}" => Block(exp :: explist)
    case "}" ~ exp ~ Nil ~ "}" => Block(List(exp))
  }


  def expression: Parser[Expression] = declaration | conditional | iteration | assignment | disjunction | failure("Invalid expression")

  //LAMBDA | BLOCK | ETC.
  def term: Parser[Expression] = deref | lambda | block | literal | identifier | "(" ~> expression <~ ")"

  def iteration: Parser[Iteration] = "while"~"("~expression~")"~expression ^^
    {
      case "while" ~ "(" ~ condition ~ ")" ~ body => Iteration(condition, body)
    }
  def assignment: Parser[Assignment] = identifier~"="~expression ^^
    {
      case id ~ "=" ~ exp => Assignment(id, exp)
    }

  def deref:Parser[Expression] = "[" ~ expression ~ "]" ^^
    {
      case "[" ~ exp ~ "]" =>  FunCall(Identifier("content"), List(exp))
    }



}
