/**
  * Bryan Garcia
  */

package values

import expressions.Literal

case class Boole(val value: Boolean) extends Literal {


  def &&(other: Boole): Boole = Boole(this.value && other.value)

  def ||(other: Boole): Boole = Boole(this.value || other.value)

  def !(): Boole = Boole(!this.value)

}
