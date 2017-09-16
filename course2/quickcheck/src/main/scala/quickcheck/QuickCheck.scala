package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    val h = const(empty)
    val data = for {
      int <- arbitrary[Int]
      d <- oneOf(h, genHeap)
    } yield d
    data
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  println(arbHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("heap2") = forAll { (h: H) =>
    val m1 = -7
    val m2 = 3
    val h1 = insert(m1, insert(m2, h))
    findMin(h1) == m1
  }

  property("heap2a") = forAll { (h: H) =>
    val m1 = -7
    val m2 = 3
    val h1 = insert(m1, insert(m2, h))
    val h2 = insert(m2, insert(m1, h))
    findMin(h1) == findMin(h2)
  }

  property("heap3") = forAll { (h: H) =>
    val m1 = -7
    val h1 = deleteMin(insert(m1, h))
    isEmpty(h1)
  }

  property("heap4") = forAll { (h: H) =>
    val m1 = -7
    val m2 = 3
    val h1 = deleteMin(insert(m1, insert(m2,h)))
    findMin(h1) == m2
  }

  property("heap5") = forAll { (h: H) =>
    val m1 = -7
    val m2 = 3
    val m3 = 5
    val m4 = 8
    val h1 = insert(m1, insert(m3, h))
    val h2 = insert(m2, insert(m4, h))
    findMin(meld(h1, h2)) == m1
  }

  property("heap6") = forAll { (h: H) =>
    val m1 = -7
    val m2 = 3
    val m3 = 5
    val m4 = 8
    val h1 = insert(m1, insert(m3, h))
    val h2 = insert(m2, insert(m4, h))
    val h3 = meld(h1, h2)

    def f(heap: H, xs: List[Int]): List[Int] = {
      if(isEmpty(heap)) xs
      else{
        f(deleteMin(heap), findMin(heap)::xs)
      }
    }

    f(h3, List()) == List(m4, m3, m2, m1)
  }
}
