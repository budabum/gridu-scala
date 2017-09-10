trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def ins(t: T) = new Cons(t, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString: String = "" + head + " -> " + tail
}

class Nil[T] extends List[T] {
  def isEmpty = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString: String = "[[NIL]]"
}

def nth[T](n: Int, xs: List[T]): T ={
  if (xs.isEmpty) throw new IndexOutOfBoundsException()
  if (n == 0) xs.head
  else nth(n-1, xs.tail)
}

val c1 = new Cons(55, new Nil[Int])
val c2 = new Cons(77, c1)
val c3 = c1.ins(77).ins(88)
val c4 = new Nil[Int].ins(9).ins(5).ins(7).ins(8).ins(9)
nth(2, c4)
//nth(5, c4)
//nth(7, c4)
new Cons(1, new Cons(3, new Cons(5, new Nil[Int])))
