import Math._

trait Task[A]{
  def join: A
  implicit def getJoin[T](x: Task[T]): T = x.join
}

def task[A](c: => A): Task[A]

def sumSegments(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  a.slice(s, t).foldLeft(0){ (sum, e) => sum + pow(abs(e), p).toInt }
}

def pNorm(a: Array[Int], p: Double): Int = {
  pow(sumSegments(a, p, 0, a.length), 1/p).toInt
}

def pNormTwoParts(a: Array[Int], p:Double): Int = {
  val m = a.length / 2
  val (s1, s2) = (sumSegments(a, p, 0, m),
                  sumSegments(a, p, m, a.length))
  pow(s1 + s2, 1/p).toInt
}

def parallel[A, B](cA: => A, cB: => B): (A, B) = {
  val tB: Task[B] = task { cB }
  val tA: A = cA
  (tA, tB.join)
}

def pNormTwoParts2(a: Array[Int], p:Double): Int = {
  val m = a.length / 2
  val (s1, s2) = parallel(sumSegments(a, p, 0, m),
                  sumSegments(a, p, m, a.length))
  pow(s1 + s2, 1/p).toInt
}

val arr1 = Array(1, 3, 5, 7, 9)
val arr2 = Array(3, 4)
sumSegments(arr1, 2, 1, 4)
pNorm(arr1, 2)
pNorm(arr2, 2)
pNormTwoParts(arr1, 2)
pNormTwoParts(arr2, 2)
//pNormTwoParts2(arr2, 2)

