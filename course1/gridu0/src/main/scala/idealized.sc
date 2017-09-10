package idealized.scala

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T
  def &&(x: Boolean): Boolean = ifThenElse(x, false)
  def ||(x: Boolean): Boolean = ifThenElse(true, x)
  def unary_!: Boolean = ifThenElse(false, true)
  def ==(x: Boolean): Boolean = ifThenElse(x, !x)
  def !=(x: Boolean): Boolean = ifThenElse(!x, x)
  def < (x: Boolean): Boolean = ifThenElse(false, x)
}

object true extends Boolean{
  def ifThenElse[T](t: => T, e: => T):T = t
}
object false extends Boolean{
  def ifThenElse[T](t: => T, e: => T):T = e
}

abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def succ: Nat = new Succ(this)
  def +(n: Nat): Nat
  def -(n: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def pred: Nat = throw new Error

  override def succ: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) this else throw new Error
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def pred: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else n - thad.pred
}

