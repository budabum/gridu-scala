def abs(x:Int): Int = if (x < 0) -x else x

class Rational(x: Int, y: Int) {
  require(y != 0, "y must be not 0")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val d = gcd(abs(x), abs(y))
  def negative: Int = if (x * y >= 0) 1 else -1
  def denom: Int = abs(y) / d
  def numer: Int = negative * abs(x) / d

  def this(x: Int) = this(x, 1)

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- = new Rational(numer * -1, denom)

  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean =
    that.numer * denom > numer * that.denom

  def max(that: Rational): Rational =
    if (this < that) that else this

  override def toString: String = {
    numer + "/" + denom
  }
}

val z1 = new Rational(-7, 8)
val z2 = new Rational(-7, -8)
val z3 = new Rational(7, -8)
val a = new Rational(2, 3)
val b = new Rational(1, 4)

a + b
-a
-z1
z2 - new Rational(1, 8)
a < b
z1 < z2
a + b
