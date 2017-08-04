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
   *   val s1 = singletonSet(1)
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
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
     // printSet(s)
    }
  }

  test("Intersection") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s2intersection = intersect(s12, s23)

      //printSet(s2intersection)

      assert(contains(s2intersection, 2), "only contains s2")
      assert(!contains(s2intersection, 3), "doesn't contain s3")
      assert(!contains(s2intersection, 1), "only contains s1")
    }
  }

  test("Difference") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2,s3)
      val s1difference = diff(s12, s23)

      //printSet(s1difference)

      assert(contains(s1difference, 1), "difference only contains 1")
      assert(!contains(s1difference, 2), "difference doesn't contain 2")
      assert(!contains(s1difference, 3), "difference doesn't contain 3")
    }
  }

  test("set with predicate") {
    new TestSets {
      val s12 = union(s1,s2)
      val s23 = union(s2, s3)
      val sall = union(s12, s23)

      //should contain all elements
      assert(contains(sall, 1), "Union 1")
      assert(contains(sall, 2), "Union 2")
      assert(contains(sall, 3), "Union 3")

      def predicate(x: Int): Boolean = x > 1

      val sPredicate = filter(sall, predicate)

      assert(!contains(sPredicate, 1))
      assert(contains(sPredicate, 2))
      assert(contains(sPredicate, 3))

    }
  }

  test("forall with predicate") {
    new TestSets {
      val s12 = union(s1,s2) //doesn't satisfy x>1 predicate
      val s23 = union(s2, s3) //satisfies x>1 predicate

      def predicate(x: Int): Boolean = x > 1

      assert(!forall(s12, predicate), "not ALL bigger than 1")
      assert(forall(s23, predicate), "all bigger than 1" )

    }
  }

  test("exists using forall") {
    new TestSets {
      val s12 = union(s1,s2) //no items that satisfy x>2 predicate
      val s23 = union(s2, s3) //at least one item that satisfies x>2 predicate

      def predicate(x: Int): Boolean = x > 2

      assert(contains(s23, 3), "one set contains the number 3")
      assert(!exists(s12, predicate), "no items satisfy x>2 predicate")
      assert(exists(s3, predicate), "at least one item satisfies x>2 predicate")
      assert(exists(s23, predicate), "at least one item satisfies x>2 predicate")
    }
  }

  test("map") {
    new TestSets {
      val s23 = union(s2,s3)

      val mapSquareSet = map(s23, x => x*x)

      assert(contains(mapSquareSet, 4), "new set contains square(2)!!")
      assert(contains(mapSquareSet, 9), "new set contains square(3)")
      assert(!contains(mapSquareSet, 2))

      val mapCubeSet = map(s23, x=> x*x*x)

      assert(contains(mapCubeSet, 8), "new set contains cube of 2 which is 8")
      assert(contains(mapCubeSet, 27), "new set contains cube of 3 which is 27")


    }
  }


}
