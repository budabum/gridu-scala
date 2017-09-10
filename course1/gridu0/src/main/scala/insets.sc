abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def union2(other: IntSet): IntSet
  def getLeft: IntSet
  def getRight: IntSet
  def getElem: Int
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  override def toString: String = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet = {
    if(other == Empty) this
    else this.incl(other.getElem).union(other.getLeft).union(other.getRight)
  }

  override def getLeft = left

  override def getRight = right

  override def getElem = elem

  override def union2(other: IntSet) = {
    ((left union2 right) union2 other) incl elem
  }
}

object Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString: String = "."
  override def union(other: IntSet) = other

  override def getLeft = Empty

  override def getRight = Empty

  override def getElem = ???

  override def union2(other: IntSet) = other
}

Empty.incl(1).incl(2).incl(3)
val is1 = new NonEmpty(1, Empty, Empty)
val is2 = is1.incl(3).incl(5).incl(7).incl(7)
val is3 = new NonEmpty(2, Empty, Empty).incl(4).incl(6).incl(8)
val is4 = is2.union(is3)
val is5 = is2.union2(is3)
val is6 = new NonEmpty(1, Empty, Empty).incl(2).incl(3).incl(4).incl(5).incl(6).incl(7).incl(8)
