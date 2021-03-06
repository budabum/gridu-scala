package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = {
    val e = elem
    (elem: Int) => elem == e
//    (elem: Int) => true
  }

  def fromRange(r: Range): Set = {
    def f(xs: List[Int], u: Set): Set = {
      if (xs.isEmpty) u
      else f(xs.tail, union(u, singletonSet(xs.head)))
    }
    val list = r.toList
    val set = singletonSet(list.head)

    f(list.tail, set)
  }

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = {
    (n: Int) => contains(s, n) || contains(t, n)
  }

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = {
    (n: Int) => contains(s, n) && contains(t, n)
  }

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = {
    (n: Int) => contains(s, n) && !contains(t, n)
  }

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = {
    (n: Int) => contains(s, n) && p(n)
  }


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(n: Int): Boolean = {
      if(n > bound) true
      else if(contains(s, n)) p(n) && iter(n+1)
      else iter(n+1)
    }

    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(n: Int): Boolean = {
      if(n > bound) false
      else if(contains(s, n) && p(n)) true
      else iter(n+1)
    }

    if(forall(s, p)) true
    else iter(-bound)
  }

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {
    def iter(n: Int, u: Set): Set = {
      if(n > bound) u
      else if(contains(s, n)) iter(n+1, union(u, singletonSet(f(n))))
      else iter(n+1, u)
    }

    iter(-bound, singletonSet(-bound-1))
  }

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
