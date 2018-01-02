import scala.collection.mutable.ListBuffer

val w = 18
val n = 4

def p(a: Any) = println(a)
val lb: ListBuffer[(Int, Int)] = ListBuffer()

val r = 0 to w

def sss(from: Int, end: Int): Int = {
  p(s"Sums of $from till ${end-1}")
  (from until end).sum
}

for {
  x <- 0 to w by n
  if x <= w
} lb += ((x, if(x+n <= w) x+n else w))

sss(3,5)

val vector = lb.toVector
vector.par.map(n => sss _ tupled n)
