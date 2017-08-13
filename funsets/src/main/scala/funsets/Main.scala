package funsets

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))
  val s1 = singletonSet(100);
  val s2 = singletonSet(1003);
  printSet(s1)
  printSet(s2)

  val bs1 = fromRange(2 to 8)
  printSet(bs1)
  printSet(map(bs1, x=>x+3))

}
