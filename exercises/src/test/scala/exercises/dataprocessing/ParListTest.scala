package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import scala.util.Random
import scala.concurrent.ExecutionContext

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  val ec: ExecutionContext = ExecutionContext.global

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples, ec)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns `None` for an empty ParList") {
    assert(minSampleByTemperature(ParList(List(), ec)) == None)
  }

  test("returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples, ec)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature the min sample exists within the partitions") {
    forAll { (samples: List[Sample]) =>
      val parSamples         = ParList.byPartitionSize(3, samples, ec)
      var maybeColdestSample = minSampleByTemperature(parSamples)

      assert(maybeColdestSample.map(samples.contains(_)).getOrElse(true))
    }
  }

  test("minSampleByTemperature behaves the same as the test oracle list") {
    forAll { (parSamples: ParList[Sample]) =>
      var maybeColdestSample       = minSampleByTemperature(parSamples)
      var expectedMinColdestSample = parSamples.partitions.flatten.minByOption(_.temperatureFahrenheit)

      assert(maybeColdestSample == expectedMinColdestSample)
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples, ec)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature returns `None` for an empty ParList") {
    assert(averageTemperature(ParList(List(), ec)) == None)
  }

  test("averageTemperature behaves the same as the test oracle list") {
    forAll { (parSamples: ParList[Sample]) =>
      var maybeAvgTemp    = averageTemperature(parSamples)
      var allSamples      = parSamples.partitions.flatten
      var expectedAvgTemp = allSamples.map(_.temperatureFahrenheit).sum / allSamples.length

      assert(maybeAvgTemp.map(avgTemp => avgTemp == expectedAvgTemp).getOrElse(true))
    }
  }

  test("averageTemperature doubles when the sample temperatures are double") {
    forAll { (parSamples: ParList[Sample]) =>
      val maybeAvgTemp = averageTemperature(parSamples)
      val doubledParSamples = parSamples.partitions.map { samples =>
        samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
      }
      val maybeDoubledAvgTemp = averageTemperature(ParList(doubledParSamples, ec))

      (maybeAvgTemp, maybeDoubledAvgTemp) match {
        case (None, None)                  => assert(parSamples.partitions.flatten.isEmpty)
        case (Some(avg), Some(doubledAvg)) => assert(Math.abs(avg * 2 - doubledAvg) < 0.00001)
        case (_, _)                        => fail()
      }
    }
  }

  test("monoFoldLeft sums correctly") {
    forAll { (parSamples: ParList[Int]) =>
      val expectedSum = parSamples.partitions.flatten.foldLeft(0)(_ + _)
      assert(parSamples.monoFoldLeft(Monoid.sum[Int]) == expectedSum)
    }
  }

  test("monoFoldLeft is stable") {
    forAll { (parSamples: ParList[Int]) =>
      val result1 = parSamples.monoFoldLeft(Monoid.sum[Int])
      val result2 = ParList(Random.shuffle(parSamples.partitions), ec).monoFoldLeft(Monoid.sum[Int])
      assert(result1 == result2)
    }
  }

  test("foldLeft is stable") {
    forAll { (parSamples: ParList[Int]) =>
      val result1 = parSamples.investigateThisFoldLeft(0)(_ + _)
      val result2 = ParList(Random.shuffle(parSamples.partitions), ec).investigateThisFoldLeft(0)(_ + _)
      assert(result1 == result2)
    }
  }

  test("foldMap is consistent with mapping over the data and then folding it") {
    forAll { (parSamples: ParList[String], mapFunc: (String) => Int) =>
      val monoid         = Monoid.sum[Int]
      val expectedResult = parSamples.map(mapFunc).monoFoldLeft(monoid)
      val actualResult   = parSamples.foldMap(mapFunc)(monoid)
      assert(actualResult == expectedResult)
    }
  }

  test("parallelFoldMap is consistent with foldMap") {
    forAll { (parSamples: ParList[String], mapFunc: (String) => Int) =>
      val monoid         = Monoid.sum[Int]
      val expectedResult = parSamples.foldMap(mapFunc)(monoid)
      val actualResult   = parSamples.parallelFoldMap(mapFunc)(monoid)
      assert(actualResult == expectedResult)
    }
  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
