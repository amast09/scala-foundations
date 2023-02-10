package exercises.dataprocessing

trait Monoid[A] {
  def default: A
  def combine(first: A, second: A): A
}

object Monoid {
  def zip[A, B](mA: Monoid[A], mB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def default: (A, B) = (mA.default, mB.default)

    def combine(first: (A, B), second: (A, B)): (A, B) =
      (mA.combine(first._1, second._1), mB.combine(first._2, second._2))
  }

  def sum[A](implicit n: Numeric[A]): Monoid[A] = new Monoid[A] {
    def default: A                      = n.zero
    def combine(first: A, second: A): A = n.plus(first, second)
  }

  val sampleSummary: Monoid[Summary] = new Monoid[Summary] {
    def default = Summary(
      max = maxSample.default,
      min = minSample.default,
      size = sum[Int].default,
      sum = sum[Double].default
    )

    def combine(s1: Summary, s2: Summary): Summary = Summary(
      max = maxSample.combine(s1.max, s2.max),
      min = minSample.combine(s1.min, s2.min),
      size = sum[Int].combine(s1.size, s2.size),
      sum = sum[Double].combine(s1.sum, s2.sum)
    )
  }

  val aggregateSummaries: Monoid[Map[String, Summary]] = new Monoid[Map[String, Summary]] {
    def default = Map.empty[String, Summary]

    // TODO: Merge Maps
    def combine(m1: Map[String, Summary], m2: Map[String, Summary]): Map[String, Summary] = {
      val combinedKeys = m1.keys.concat(m2.keys).toSet

      combinedKeys.foldLeft(Map.empty[String, Summary]) { (acc, key) =>
        (m1.get(key), m2.get(key)) match {
          case (Some(s1), Some(s2)) => acc + (key -> sampleSummary.combine(s1, s2))
          case (None, None)         => acc
          case (Some(s), None)      => acc + (key -> s)
          case (None, Some(s))      => acc + (key -> s)
        }
      }
    }
  }

  def compareSample(compare: (Sample, Sample) => Sample): Monoid[Option[Sample]] = new Monoid[Option[Sample]] {
    def default = Option.empty[Sample]
    def combine(first: Option[Sample], second: Option[Sample]): Option[Sample] = (first, second) match {
      case (Some(sample1), Some(sample2)) => Some(compare(sample1, sample2))
      case (None, None)                   => Option.empty[Sample]
      case (Some(s), None)                => Some(s)
      case (None, Some(s))                => Some(s)
    }
  }

  val minSample: Monoid[Option[Sample]] = compareSample { (sample1, sample2) =>
    if (sample1.temperatureFahrenheit < sample2.temperatureFahrenheit) sample1
    else sample2
  }

  val maxSample: Monoid[Option[Sample]] = compareSample { (sample1, sample2) =>
    if (sample1.temperatureFahrenheit > sample2.temperatureFahrenheit) sample1
    else sample2
  }
}
