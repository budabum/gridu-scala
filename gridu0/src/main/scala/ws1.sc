import scala.annotation.tailrec

def sqrt(x: Double) = {
  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) = {
    abs(guess * guess - x) / x < 0.0001
  }

  def improve(guess: Double) =(guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(4)
sqrt(1e-6)
sqrt(1e-20)
sqrt(1e60)

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)
gcd(2, 7)
gcd(13, 23)
gcd(123433214, 12432142)

def fac1(n: Int): Int = {
  if (n == 0) 1 else n * fac1(n-1)
}

def fac2(n: Int): Int = {
  @tailrec
  def f(res: Int, n: Int): Int ={
    if(n == 1) res else f(res * n, n-1)
  }

  f(1, n)
}

(1 to 5).map(z => fac1(z)).toList
(1 to 5).map(z => fac2(z)).toList



