package exercises.dataprocessing

import exercises.dataprocessing.StackSafeRecursiveExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.util.Try

class StackSafeRecursiveExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  val largeSize = 100000

  test("unsafeSum is not stack-safe") {
    try {
      unsafeSum(List.fill(largeSize)(0))
      fail("Expected stack overflow")
    } catch {
      case _: StackOverflowError => succeed
      case e: Throwable          => fail(e)
    }
  }

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
    assert(sum(List.fill(largeSize)(0)) == 0)
  }

  test("sum is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("min") {
    assert(min(List(1, 5, 2)) == Some(1))
    assert(min(Nil) == None)
    assert(Try(min(List.fill(largeSize)(0))).isSuccess)
  }

  test("min is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(min(numbers) == numbers.minOption)
    }
  }

  test("min is the smallest number") {
    forAll { (numbers: List[Int]) =>
      val maybeMinValue = min(numbers)
      assert(numbers.forall(v => maybeMinValue.map(minValue => minValue <= v).getOrElse(false)))
    }
  }

  test("reverse") {
    assert(reverse(List(1, 5, 2)) == List(2, 5, 1))
    assert(reverse(Nil) == List())
    assert(Try(reverse(List.fill(largeSize)(0))).isSuccess)
  }

  test("reverse is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(reverse(numbers) == numbers.reverse)
    }
  }

  test("reverse produces the same sized list") {
    forAll { (numbers: List[Int]) =>
      assert(reverse(numbers).length == numbers.length)
    }
  }

  test("foldLeft") {
    val sum = (acc: Int, v: Int) => acc + v
    assert(foldLeft(List(1, 5, 2), 0)(sum) == 8)
    assert(foldLeft(List(), 0)(sum) == 0)
    assert(Try(reverse(List.fill(largeSize)(0))).isSuccess)
  }

  test("foldLeft is consistent with std library") {
    forAll { (numbers: List[Int], combine: (Int, Int) => Int) =>
      assert(foldLeft(numbers, 0)(combine) == numbers.foldLeft(0)(combine))
    }
  }
}
