/**
  * Created by Bryan Garcia
  */

package ui

class SyntaxException(val result: Parsers#Failure = null) extends GeneralException("Syntax error"){
  override def toString():String = result.toString
}
