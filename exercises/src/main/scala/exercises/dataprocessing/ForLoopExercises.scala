package exercises.dataprocessing

object ForLoopExercises {

  def sum(numbers: List[Int]): Int =
    foldLeft[Int, Int](0)(_ + _)(numbers)

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int =
    foldLeft[Int, A](0)((acc, _) => acc + 1)(items)

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] =
    generalMin(numbers)

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] =
    foldLeft[Map[String, Int], String](Map.empty) { (wordCounts, word) =>
      val currentCountForWord = wordCounts.get(word).getOrElse(0)
      wordCounts + (word -> (currentCountForWord + 1))
    }(words)

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def foldLeft[Acc, Elem](initialAccValue: Acc)(accFunction: (Acc, Elem) => Acc)(elements: List[Elem]): Acc = {
    var finalAccValue = initialAccValue

    for (element <- elements)
      finalAccValue = accFunction(finalAccValue, element)

    finalAccValue
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    foldLeft[List[To], From](List())(_ :+ update(_))(elements)

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    foldLeft[List[A], A](List())((xs, s) => s +: xs)(elements)

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    foldLeft[Option[A], A](None)((acc, el) => Some(el))(elements)

  // g. Can you generalize `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(implicit ord: math.Ordering[A]): Option[A] =
    foldLeft[Option[A], A](None) {
      case (Some(value), elem) if ord.compare(value, elem) < 0 =>
        Some(value)
      case (_, elem) => Some(elem)
    }(elements)

}
