package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]], ec: ExecutionContext) {
  def monoFoldLeft(monoid: Monoid[A]): A =
    partitions
      .map(pList => pList.foldLeft(monoid.default)(monoid.combine))
      .foldLeft(monoid.default)(monoid.combine)

  def map[B](mapFunc: A => B): ParList[B] =
    copy(partitions = partitions.map(_.map(mapFunc)))

  def size: Int =
    parallelFoldMap(_ => 1)(Monoid.sum[Int])

  def foldMap[To](mapFunc: A => To)(monoid: Monoid[To]): To =
    partitions
      .map((partition: List[A]) =>
        partition.foldLeft(monoid.default)((acc: To, a: A) => monoid.combine(acc, mapFunc(a)))
      )
      .foldLeft(monoid.default)(monoid.combine)

  def parallelFoldMap[To](mapFunc: A => To)(monoid: Monoid[To]): To = {
    implicit val executionContext: ExecutionContext = ec

    val partitionResultFutures = partitions
      .map { (partition: List[A]) =>
        Future {
          partition.foldLeft(monoid.default)((acc, a) => monoid.combine(acc, mapFunc(a)))
        }
      }
    val futurePartitionResults = Future.sequence(partitionResultFutures)
    val futureResult           = futurePartitionResults.map(_.foldLeft(monoid.default)(monoid.combine))

    Await.result(futureResult, Duration.Inf)
  }

  //---------

  def investigateThisFoldMap[To](mapFunc: A => To)(monoid: Monoid[To]): To =
    partitions
      .foldLeft(monoid.default)((to: To, partition: List[A]) =>
        partition.foldLeft(to)((to: To, a: A) => monoid.combine(to, mapFunc(a)))
      )

  def investigateThisFoldLeft[To](default: To)(combine: (To, A) => To): To =
    partitions
      .foldLeft(default)((to: To, partition: List[A]) => partition.foldLeft(to)(combine))

}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](ec: ExecutionContext, partitions: List[A]*): ParList[A] =
    ParList(partitions.toList, ec)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A], ec: ExecutionContext): ParList[A] =
    if (items.isEmpty) ParList(ec)
    else ParList(items.grouped(partitionSize).toList, ec)
}
