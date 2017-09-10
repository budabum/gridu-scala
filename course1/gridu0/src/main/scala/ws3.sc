val tolerance = 0.0001

def abs(x:Double) = if (x < 0) -x else x

def isCloseEnough(x: Double, y: Double) =
  abs((x-y)/x)/x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double ={
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1+x/2)(88)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

averageDamp(x => x*x)(2)
averageDamp(x => x+x)(4)

def sqrt1(x: Double) = {
  fixedPoint(averageDamp(y => x/y))(2)
}
sqrt1(4)
sqrt1(25)
