package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  def localDateGen: Gen[LocalDate] =
    Gen
      .choose(
        min = LocalDate.MIN.toEpochDay,
        max = LocalDate.MAX.toEpochDay
      )
      .map(LocalDate.ofEpochDay)

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(1, 2).swap == Pair(2, 1))
  }

  test("Pair map") {
    assert(Pair(3, 4).map(identity) == Pair(3, 4))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
    val replicate: (Int, String) => String = (numReplications, s) => List.fill(numReplications)(s).mkString
    assert(Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World "))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    forAll { (eval: Int => Boolean, i: Int) =>
      val genPredicate = Predicate(eval)
      assert((Predicate.True && genPredicate)(i) == eval(i))
      assert((genPredicate && Predicate.True)(i) == eval(i))
      assert((genPredicate && Predicate.False)(i) == false)
      assert((Predicate.False && genPredicate)(i) == false)
    }
  }

  test("Predicate ||") {
    forAll { (eval: Int => Boolean, i: Int) =>
      val genPredicate = Predicate(eval)
      assert((Predicate.True || genPredicate)(i))
      assert((genPredicate || Predicate.True)(i))
      assert((genPredicate || Predicate.False)(i) == eval(i))
      assert((Predicate.False || genPredicate)(i) == eval(i))
    }
  }

  test("Predicate flip") {
    assert(Predicate.True.flip(()) == false)
    assert(Predicate.False.flip(()))
  }

  test("isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(isValidUser(User("John", 17)) == false) // user is not an adult
    assert(isValidUser(User("john", 20)) == false) // name is not capitalized
    assert(isValidUser(User("x", 23)) == false)    // name is too small
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    forAll { (i: Int) =>
      assert(userIdDecoder.decode(i.toString) == UserId(i))
    }
    assert(Try(userIdDecoder.decode("NOT AN INT")).isFailure)
  }

  test("JsonDecoder LocalDate") {
    forAll(localDateGen) { (d: LocalDate) =>
      assert(localDateDecoder.decode("\"" + d.toString + "\"") == d)
    }
    assert(Try(localDateDecoder.decode("NOT AN DATE")).isFailure)
  }

  test("JsonDecoder map") {
    forAll { (i: Int, update: Int => Int) =>
      val s = i.toString
      assert(intDecoder.map(update).decode(s) == update(intDecoder.decode(s)))
      assert(intDecoder.map(identity).decode(s) == intDecoder.decode(s))
    }
  }

  test("JsonDecoder orElse") {
    val hardCodedIntDecoder: JsonDecoder[Int] = (s: Json) => 5
    assert(intDecoder.orElse(hardCodedIntDecoder).decode("5") == 5)
    assert(intDecoder.orElse(hardCodedIntDecoder).decode("INVALID INT") == 5)
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll(localDateGen) { (d: LocalDate) =>
      assert(weirdLocalDateDecoder.decode("\"" + d.toString + "\"") == d)
      assert(weirdLocalDateDecoder.decode(d.toEpochDay.toString) == d)
    }
    assert(Try(weirdLocalDateDecoder.decode("NOT AN DATE")).isFailure)
  }

  test("optionDecoder") {
    val optionIntDecoder = optionDecoder(intDecoder)
    assert(optionIntDecoder.decode("NOT AN INT") == None)
    assert(optionIntDecoder.decode("5") == Some(5))
  }
}
