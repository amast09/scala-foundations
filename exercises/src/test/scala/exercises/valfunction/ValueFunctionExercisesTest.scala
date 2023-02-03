package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // -- selectDigits

  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits every character is a digit") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  // -- secret

  test("secret examples") {
    assert(secret("hello4world-80") == "**************")
    assert(secret("welcome") == "*******")
  }

  test("secret length is always the same") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret every character is a *") {
    forAll { (text: String) =>
      secret(text).foreach(c => assert(c == '*'))
    }
  }

  test("secret is idempotent") {
    forAll { (text: String) =>
      val secretAppliedOnce  = secret(text)
      val secretAppliedTwice = secret(secret(text))
      assert(secretAppliedOnce == secretAppliedTwice)
    }
  }

  // -- isValidUsernameCharacter

  test("isValidUsernameCharacter returns true for lowercase letters") {
    assert(('a' to 'z').forall(isValidUsernameCharacter))
  }

  test("isValidUsernameCharacter returns true for uppercase letters") {
    assert(('A' to 'Z').forall(isValidUsernameCharacter))
  }

  test("isValidUsernameCharacter returns true for digits") {
    assert(('0' to '9').forall(isValidUsernameCharacter))
  }

  test("isValidUsernameCharacter returns true for the `-` and `_` characters") {
    assert(List('-', '_').forall(isValidUsernameCharacter))
  }

  test("isValidUsernameCharacter returns false for invalid characters") {
    assert(List('&', '^', '}').forall(c => !isValidUsernameCharacter(c)))
  }

  // -- isValidUsername
  test("isValidUsername returns true for valid usernames") {
    assert(isValidUsername("john-doe"))
  }

  test("isValidUsername returns false for an invalid usernames") {
    assert(isValidUsername("*john*") == false)
  }

  test("isValidUsername returns false when an invalid character is in the username") {
    forAll { (text: String) =>
      val isTextAValidUsername        = isValidUsername(text)
      val isReverseTextAValidUsername = isValidUsername(text.reverse)
      assert(isTextAValidUsername == isReverseTextAValidUsername)
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  // -- isPositive

  test("Point.isPositive returns true when x y and z are all positive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point.isPositive returns false when x y and z are not all positive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(1) * -1, y.max(0), z.max(0)).isPositive == false)
      assert(Point(x.max(0), y.max(1) * -1, z.max(0)).isPositive == false)
      assert(Point(x.max(0), y.max(0), z.max(1) * -1).isPositive == false)
      assert(Point(x.max(1) * -1, y.max(1) * -1, z.max(0)).isPositive == false)
      assert(Point(x.max(1) * -1, y.max(0), z.max(1) * -1).isPositive == false)
      assert(Point(x.max(0), y.max(1) * -1, z.max(1) * -1).isPositive == false)
      assert(Point(x.max(1) * -1, y.max(1) * -1, z.max(1) * -1).isPositive == false)
    }
  }

  // -- isEven

  def makeEven(v: Int): Int = v - (v % 2)
  def makeOdd(v: Int): Int  = makeEven(v) + 1

  test("Point.isEven returns true when x y and z are all even") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(makeEven(x), makeEven(y), makeEven(z)).isEven)
    }
  }

  test("Point.isEven returns false when x y and z are not all even") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(makeOdd(x), makeEven(y), makeEven(z)).isEven == false)
      assert(Point(makeOdd(x), makeOdd(y), makeEven(z)).isEven == false)
      assert(Point(makeOdd(x), makeOdd(y), makeOdd(z)).isEven == false)
      assert(Point(makeOdd(x), makeEven(y), makeOdd(z)).isEven == false)
      assert(Point(makeEven(x), makeEven(y), makeOdd(z)).isEven == false)
    }
  }

  // -- forAll

  test("Point.forAll returns the correct boolean if all elements satisfy or dissatisfy the predicate") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }
}
