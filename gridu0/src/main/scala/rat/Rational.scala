package rat

/**
  * @author Alexey Lyanguzov.
  */
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  override def toString = numer + "/" + denom
}
