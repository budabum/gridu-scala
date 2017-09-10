package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val bs1 = fromRange(1 to 10)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains") {
    new TestSets {
      val a1 = union(s1, s2)
      val a2 = union(s1, s3)
      var a3 = intersect(a1, a2)
      assert(contains(a3, 1), "Intersect 1")
      assert(!contains(a3, 3), "Intersect 3")
    }
  }

  test("diff contains") {
    new TestSets {
      val d1 = union(s1, s2)
      val d2 = union(s1, s3)
      var d3 = diff(d1, d2)
      val d4 = diff(d2, d1)
      assert(contains(d3, 2), "Diff 1")
      assert(contains(d4, 3), "Diff 2")
      assert(!contains(d3, 3), "Diff 3")
      assert(!contains(d4, 2), "Diff 4")
    }
  }

  test("filter contains") {
    new TestSets {
      val f1 = filter(bs1, x => x % 2 == 0)
      assert(contains(bs1, 1), "Filter 1")
      assert(contains(bs1, 2), "Filter 2")
      assert(contains(f1, 2), "Filter 3")
      assert(contains(f1, 10), "Filter 4")
      assert(!contains(f1, 1), "Filter 5")
      assert(!contains(f1, 11), "Filter 6")
      assert(!contains(f1, 7), "Filter 7")
    }
  }

  test("forall contains") {
    new TestSets {
      val f1 = filter(bs1, x => x % 2 == 0)
      assert(forall(bs1, x => x <= 10), "ForAll 1")
      assert(forall(bs1, x => x > 0), "ForAll 2")
      assert(!forall(bs1, x => x < 0), "ForAll 3")
      assert(!forall(bs1, x => x > 10), "ForAll 4")
      assert(!forall(bs1, x => x == 7), "ForAll 5")
      assert(forall(f1, x => x % 2 == 0), "ForAll 6")
      assert(!forall(f1, x => x % 2 != 0), "ForAll 7")
    }
  }

  test("exists contains") {
    new TestSets {
      val f1 = filter(bs1, x => x % 2 == 0)
      assert(exists(bs1, x => x <= 10), "Exists 1")
      assert(exists(bs1, x => x > 0), "Exists 2")
      assert(!exists(bs1, x => x < 0), "Exists 3")
      assert(!exists(bs1, x => x > 10), "Exists 4")
      assert(exists(bs1, x => x == 7), "Exists 5")
      assert(exists(f1, x => x % 2 == 0), "Exists 6")
      assert(!exists(f1, x => x % 2 != 0), "Exists 7")
      assert(exists(f1, x => x == 4), "Exists 8")
      assert(!exists(f1, x => x == 7), "Exists 9")
    }
  }

  test("map contains") {
    new TestSets {
      val m1 = map(bs1, x => x * 2)
      val m2 = map(bs1, x => if (x % 2 == 0) x else -1)
      assert(contains(m1, 20), "Map 1")
      assert(contains(m2, -1), "Map 2")
      assert(forall(m1, x => (2 to 20 by 2).contains(x)), "Map 3")
      assert(contains(m2, 4), "Map 4")
      assert(!contains(m2, 5), "Map 5")
      assert(!contains(m2, 11), "Map 6")
      assert(!contains(m1, 11), "Map 7")
      assert(contains(m1, 4), "Map 8")
    }
  }

}
