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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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
    val evens: Set = _ % 2 == 0
    val integers: Set = _ => true
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
      assert(!contains(s1, 2), "Wrong singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains common elements") {
    new TestSets {
      val s = intersect(union(s1, s2), union(s2, s3))
      assert(!contains(s, 1), "1 is excluded from the intersection")
      assert(contains(s, 2), "2 is the common element")
      assert(!contains(s, 3), "3 is excluded from the intersection")
    }
  }

  test("diff contains different elements") {
    new TestSets {
      val s = diff(union(s1, s2), union(s2, s3))
      val t = diff(union(s3, s2), union(s2, s1))
      assert(contains(s, 1), "1 is in the first set only")
      assert(!contains(s, 2), "2 is in both sets")
      assert(!contains(s, 3), "3 is not in the first set")
      assert(contains(t, 3), "3 is in the first set only")
      assert(!contains(t, 1), "1 is not in the first set")
      assert(!contains(s, 2), "2 is in both sets")
    }
  }

  test("filter contains a select subset of elements") {
    new TestSets {
      val s = union(s1, union(s2, s3))
      val even = filter(s, _ % 2 == 0)
      val odd = filter(s, _ % 2 == 1)
      assert(!contains(even, 1), "1 is not even")
      assert(contains(odd, 1), "1 is odd")
      assert(contains(even, 2), "2 is even")
      assert(!contains(odd, 2), "2 is not odd")
      assert(!contains(even, 3), "3 is not even")
      assert(contains(odd, 3), "3 is odd")
    }
  }

  test("forall verifies a predicate over an entire set") {
    new TestSets {
      assert(forall(evens, _ % 2 == 0), "all even numbers are divisible by 2")
      assert(!forall(evens, _ > 0), "not all even numbers are positive")
    }
  }

  test("exists verifies existence of a valid element in a set") {
    new TestSets {
      assert(exists(evens, _ < 0), "at least one even number is negative")
      assert(!exists(evens, _ % 2 != 0),
        "there does not exist an even number not divisible by 2")
    }
  }

  test("map transforms a set into another set") {
    new TestSets {
      assert(forall(map(integers, _ + 1), contains(integers, _)),
        "incrementing every integer results in the same set")
      assert(forall(map(integers, _ * 2), _ % 2 == 0),
        "doubling every integer results in all even numbers")
    }
  }
}
