import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

lazy val genMap: Gen[Map[Int,Int]] = oneOf(
  const(Map.empty[Int,Int]),
  for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
)


println(genMap.sample)
println(genMap.sample)
println(genMap.sample)
println(genMap.sample)
println(genMap.sample)
println(genMap.sample)

sealed abstract class Tree
case class Node(left: Tree, right: Tree, v: Int) extends Tree
case object Leaf extends Tree

val genLeaf = const(Leaf)

val genNode = for {
  v <- arbitrary[Int]
  left <- genTree
  right <- genTree
} yield Node(left, right, v)

def genTree: Gen[Tree] = oneOf(genLeaf, genNode)
println(genTree.sample)

val genB = Gen.frequency( (2, true), (1, false) )
(1 to 12).foreach(e => println(genB.sample))
