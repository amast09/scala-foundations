package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import Monoid._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class MonoidTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  def validateMonoid[A](name: String, param: Monoid[A], gen: Gen[A]) = {
    test(s"${name} - combine with the default value is a noop") {
      forAll(gen) { (value: A) =>
        assert(param.combine(param.default, value) == value)
        assert(param.combine(value, param.default) == value)
      }
    }

    test(s"${name} - combine is associative") {
      forAll(gen, gen, gen) { (value1: A, value2: A, value3: A) =>
        val applicationOrder1 = param.combine(param.combine(value1, value2), value3)
        val applicationOrder2 = param.combine(value1, param.combine(value2, value3))
        assert(applicationOrder1 == applicationOrder2)
      }
    }
  }

  val restrictedDoubleGen: Gen[Double]     = Gen.choose(-200f, 200f).map(_.toDouble)
  val intGen: Gen[Int]                     = Gen.choose(Int.MinValue, Int.MaxValue)
  val stringGen: Gen[String]               = Gen.alphaStr
  val optionSampleGen: Gen[Option[Sample]] = Gen.option(sampleGen)

  validateMonoid("sumInt", Monoid.sum[Int], intGen)
  validateMonoid("sumDouble", Monoid.sum[Double], restrictedDoubleGen)
  validateMonoid(
    "sumIntDoubleTuple",
    Monoid.zip(Monoid.sum[Int], Monoid.sum[Double]),
    Gen.zip(intGen, restrictedDoubleGen)
  )
  validateMonoid("minSample", Monoid.minSample, optionSampleGen)
  validateMonoid("maxSample", Monoid.minSample, optionSampleGen)
  validateMonoid("sampleSummary", Monoid.sampleSummary, summaryGen)
  validateMonoid("aggregateSummaries", Monoid.aggregateSummaries, Gen.mapOf(Gen.zip(stringGen, summaryGen)))

  validateMonoid("zip", Monoid.zip(Monoid.sum[Int], Monoid.sum[Int]), Gen.zip(intGen, intGen))
}
