package exercises.dataprocessing

import scala.annotation.tailrec

object StackSafeRecursiveExercises {

  def unsafeSum(numbers: List[Int]): Int =
    numbers match {
      case Nil          => 0
      case head :: tail => head + unsafeSum(tail)
    }

  def sum(numbers: List[Int]): Int = {
    @tailrec
    def go(numbers: List[Int], accumulator: Int): Int =
      numbers match {
        case Nil          => accumulator
        case head :: tail => go(tail, accumulator + head)
      }
    go(numbers, 0)
  }

  // a. Implement `min` using a recursion
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  def min(numbers: List[Int]): Option[Int] = {
    @tailrec
    def go(numbers: List[Int], maybeCurrentMin: Option[Int]): Option[Int] =
      numbers match {
        case Nil => maybeCurrentMin
        case head :: tail =>
          val nextMin = maybeCurrentMin match {
            case None        => Some(head)
            case Some(value) => Some(head min value)
          }

          go(tail, nextMin)
      }

    go(numbers, None)
  }

  // b. Implement `reverse` using a recursion
  // such as reverse(List(2,5,1,8)) == List(8,1,5,2)
  // and     reverse(Nil) == Nil
  // Note: Ensure size is stack-safe
  def reverse[A](items: List[A]): List[A] = {
    def go(items: List[A], reversedList: List[A]): List[A] =
      items match {
        case Nil          => reversedList
        case head :: tail => go(tail, head +: reversedList)
      }

    go(items, Nil)
  }

  // c. Implement `foldLeft` using a recursion
  // Note: Ensure size is stack-safe
  def foldLeft[From, To](items: List[From], default: To)(combine: (To, From) => To): To = {
    @tailrec
    def go(items: List[From], accumulator: To): To =
      items match {
        case Nil          => accumulator
        case head :: tail => go(tail, combine(accumulator, head))
      }
    go(items, default)
  }

}
