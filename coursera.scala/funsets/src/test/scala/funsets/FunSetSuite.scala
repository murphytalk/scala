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
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
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
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)

    val s1234  = union(union(s1,s2),union(s3,s4))    
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      
      assert(contains(s1234, 1), "All Union 1")
      assert(contains(s1234, 2), "All Union 2")
      assert(contains(s1234, 2), "All Union 3")
      assert(contains(s1234, 2), "All Union 4")      
    }
  }
  
  test("intersect test"){
    new TestSets{
       val s = union(s1,s2)
       val inters = intersect(s1234,s)
       assert(contains(inters, 1), "intersect has 1")       
       assert(contains(inters, 2), "intersect has 2")       
       assert(!contains(inters, 3), "intersect does not have 3")       
       assert(!contains(inters, 4), "intersect does not have 4")       
    }
  }
  
  test("diff test"){
    new TestSets{      
      val s = union(singletonSet(5),union(s3,s4))
      val f = diff(s1234,s)
      assert(contains(f, 1), "diff has 1")
      assert(contains(f, 2), "diff has 2")
      assert(!contains(f, 3), "diff does not have 3")
      assert(!contains(f, 4), "diff does not have 4")
      assert(!contains(f, 5), "diff does not have 5")
    }
  }
  
  test("filter test"){
    new TestSets{
      val s12345 = union(singletonSet(5),s1234)
      val s = filter(s1234,x=>x%2==0)
      
      assert(!contains(s, 1), "filter does not have 1")
      assert(contains(s, 2), "filter does have 2")
      assert(!contains(s, 3), "filter does not have 3")
      assert(contains(s, 4), "filter does have 4")
      assert(!contains(s, 5), "filter does not have 5")
    }
  }
  
  test("forall test"){
    new TestSets{
      assert(forall(x=>x>1 && x<100,x=>x>0),"all values between 1~100 is larger than 0")
      assert(!forall(x=>x>1 && x<100,x=>x%2==0),"not any values between 1~100 is even")
      assert(forall(x=>x%2==0,x=>x%2==0),"even values")
      assert(!forall(x=>x > -5000 && x<5000,x=> x>0),"has negative 1")
      assert(!forall(x=>x > -500 && x<500,x=> x>0),"has negative 2")
    }
  }
  
  test("exists test"){
    new TestSets{
      assert(exists(x=>x>0 && x<10, x=> x%3==0),"exists x%3==0 between 0~10")
      assert(!exists(x=>x>0 && x<3, x=> x%3==0),"no x%3==0 between 0~3")
    }
  }
}
