package exercises.dataprocessing

import scala.concurrent.ExecutionContext

case class AverageValues(totalTemperature: Double, numberOfSamples: Int)

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] =
    samples.parallelFoldMap(Option(_))(Monoid.minSample)

  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
    val (numberOfSamples, totalTemp) =
      samples.parallelFoldMap(s => (1, s.temperatureFahrenheit))(Monoid.zip(Monoid.sum[Int], Monoid.sum[Double]))

    if (numberOfSamples == 0) {
      None
    } else {
      Some(totalTemp / numberOfSamples)
    }
  }

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parallelFoldMap(Option(_))(Monoid.minSample),
      max = samples.parallelFoldMap(Option(_))(Monoid.maxSample),
      sum = samples.parallelFoldMap(_.temperatureFahrenheit)(Monoid.sum[Double]),
      size = samples.parallelFoldMap(_ => 1)(Monoid.sum[Int])
    )

  def sampleToSummary(s: Sample): Summary = Summary(
    max = Option(s),
    min = Option(s),
    size = 1,
    sum = s.temperatureFahrenheit
  )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    samples.parallelFoldMap(sampleToSummary)(Monoid.sampleSummary)

  def sampleToOutput(getAggregationKeys: Sample => List[String])(sample: Sample): Map[String, Summary] = {
    val summary = sampleToSummary(sample)
    getAggregationKeys(sample).map(k => (k, summary)).toMap
  }

  def aggregateSummary(getAggregationKey: Sample => List[String])(samples: ParList[Sample]): Map[String, Summary] =
    samples.parallelFoldMap(sampleToOutput(getAggregationKey))(Monoid.aggregateSummaries)
}
