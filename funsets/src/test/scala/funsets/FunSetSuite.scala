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
    val s1:Set = singletonSet(1)
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
      val s:Set = union(s1, s2)
      println(s.toString())
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect elements of each set") {
    new TestSets {
      val s:Set = union(s1, s2)
      val one:Set = union(s1,s3)
      val two:Set = singletonSet(199)
      val three:Set = intersect(one, two)
      val four:Set = intersect(s, one)

      assert(contains(four, 1), "Union 1")
      assert(!contains(three, 199), "Union 2")
      assert(!contains(four, 2), "Union 3")
    }
  }

  test("test diff") {
    new TestSets {
      val s:Set = union(s1, s2) // 1,2
      val one:Set = union(s1,s3) // 1,3
      val two:Set = singletonSet(199) // 199
      val four:Set = union(s, one) // 1,2,3
      val five:Set = diff(four, s) // 3

      assert(contains(five, 3), "Union 1")
      assert(!contains(five, 2), "Union 3")
    }
  }

  test("test filter") {
    new TestSets {
      val s:Set = union(s1, s2) // 1,2
      val two:Set = singletonSet(199) // 199
      val four:Set = union(s, two) // 1,2,199
      val five:Set = filter(four, _ < 5) // 3

      assert(contains(five, 2), "Union 1")
      assert(!contains(five, 199), "Union 3")
      assert(!contains(five, -1), "filter -1")
    }
  }

//  forall: {1,3,4,5,7,1000}
  test("test forAll") {
    new TestSets {
      val s:Set = union(s1, s2) // 1,2
      val two:Set = singletonSet(199) // 199
      val four:Set = union(s, two) // 1,2,199
      val five:Boolean = forall(four, _ < 5) // false
      val six:Boolean = forall(s, _ < 5) // true
      printSet(s)
      assert(!five, "forAll false")
      assert(six, "for all true")
    }
  }


}