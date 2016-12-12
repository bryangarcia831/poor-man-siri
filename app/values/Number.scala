/**
  * Bryan Garcia
  */

package values

import expressions._

case class Number(val value: Double) extends Literal with Value{

  def +(other: Number): Number = Number(this.value + other.value)

  def -(other: Number): Number = Number(this.value - other.value)

  def /(other: Number): Number = Number(this.value / other.value)

  def *(other: Number): Number = Number(this.value * other.value)

  def <(other: Number): Boole = Boole(this.value < other.value)

  def >(other: Number): Boole = Boole(this.value > other.value)

  def <=(other: Number): Boole = Boole(this.value <= other.value)

  def >=(other: Number): Boole = Boole(this.value >= other.value)


  def ==(other: Number): Boole = Boole(this.value == other.value)

  def !=(other: Number): Boole = Boole(this.value != other.value)

  override def toString: String = {
    value + ""
  }

}
