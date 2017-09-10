val st1 = (1 to 100).toStream

def expr = {
  val x = { println('x'); 1 }
  lazy val y = { println('y'); 2 }
  def z = { println ('z'); 3 }
  z + y + x + z + y + x
}

expr

def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)

val squares = nats map(_*4)

squares.take(4).toList

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter(_ % s.head != 0))
}

val primes = sieve(from(2))

primes.take(7).toList

