package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size is consistent with List size") {
    forAll { (numbers: List[Int]) =>
      assert(size(numbers) == numbers.size)
    }
  }

  test("the size of 2 lists combined is consistent") {
    forAll { (list1: List[Int], list2: List[Int]) =>
      assert(size(list1) + size(list2) == size(list1 ++ list2))
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
    assert(min(List()) == None)
  }

  test("min returns the smallest value in the list") {
    forAll { (list: List[Int]) =>
      min(list) match {
        case None            => assert(list.isEmpty)
        case Some(minOfList) => assert(list.forall(minOfList <= _))
      }
    }
  }

  test("min returns an element contained in the list") {
    forAll { (list: List[Int]) =>
      assert(
        min(list)
          .map(minOfList => list.contains(minOfList))
          .getOrElse(true)
      )
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount returns a map with all elements") {
    forAll { (list: List[String]) =>
      val wordCounts = wordCount(list)
      list.foreach { e =>
        assert(wordCounts.get(e).map(_ => true).getOrElse(false))
      }
    }
  }

  test("wordCount adds up to size of list") {
    forAll { (list: List[String]) =>
      val wordCounts = wordCount(list)
      val totalCount = wordCounts.values.foldLeft(0)(_ + _)
      assert(totalCount == list.length)
    }
  }

  test("wordCount all frequencies are positive") {
    forAll { (list: List[String]) =>
      assert(wordCount(list).values.forall(_ > 0))
    }
  }

  test("foldLeft processes elements left to right") {
    forAll { (list: List[String]) =>
      assert(foldLeft[List[String], String](List())(_ :+ _)(list) == list)
    }
  }

  test("map") {
    forAll { (list: List[String], mapFunc: String => String) =>
      val mappedList = map(list)(mapFunc)

      mappedList.zipWithIndex.foreach { case (mappedElem, idx) =>
        assert(mappedElem == mapFunc(list(idx)))
      }
    }
  }

  test("reverse") {
    forAll { (list: List[String], mapFunc: String => String) =>
      val mappedList = reverse(list)

      mappedList.zipWithIndex.foreach { case (mappedElem, idx) =>
        assert(mappedElem == list((list.length - 1) - idx))
      }
    }
  }

  test("lastOption") {
    forAll { (list: List[String]) =>
      lastOption(list) match {
        case None        => assert(list.isEmpty)
        case Some(value) => assert(value == list(list.length - 1))
      }
    }
  }

  test("generalMin") {
    assert(generalMin(List(2, 5, 1, 8)) == Some(1))
    assert(generalMin(List('A', 'B', 'C')) == Some('A'))
    assert(generalMin(List("Foo", "Bar", "Baz")) == Some("Bar"))
    assert(generalMin[Int](List()) == None)
  }
}
