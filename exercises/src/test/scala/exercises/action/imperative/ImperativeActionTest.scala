package exercises.action.imperative

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.ImperativeActionTest
class ImperativeActionTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("retry when maxAttempt is 0") {
    val result = Try(retry(0)(""))
    assert(result.isFailure)
  }

  test("retry when action fails") {
    var counter = 0
    val error   = new Exception("Boom")

    val result = Try(retry(5) {
      counter += 1
      throw error
    })

    assert(result == Failure(error))
    assert(counter == 5)
  }

  test("retry until action succeeds") {
    var counter = 0
    val result = Try(retry(5) {
      counter += 1
      require(counter >= 3, "Counter is too low")
      "Hello"
    })
    assert(result == Success("Hello"))
    assert(counter == 3)
  }

  test("onError executes the `cleanup` function when the action errors") {
    val expectedResult = new Exception("BANG!")
    var counter        = 0
    val result = Try(
      onError(
        throw expectedResult,
        (e: Throwable) => counter += 1
      )
    )

    assert(counter == 1)
    assert(result == Failure(expectedResult))
  }

  test("onError does not execute the `cleanup` function when the action is successful") {
    val expectedResult = 1991
    var counter        = 0
    val result = onError(
      expectedResult,
      (e: Throwable) => counter += 1
    )

    assert(counter == 0)
    assert(result == expectedResult)
  }
}
