import java.time.Instant
import java.time.Duration
import java.time.format.FormatStyle
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import exercises.generic.GenericFunctionExercises._

println("Hello world")
println(secret.map(_.map(_.toChar).reverse.mkString).swap)

val replicate: (Int, String) => String = (numReplications, s) => List.fill(numReplications)(s).toString()

replicate(3, "hello ")

val d = LocalDate.now()

val f  = DateTimeFormatter.ofPattern("dd-MM-yyyy")
val d2 = LocalDate.parse("22-03-1991", f)

f.format(d)

"\"" + d.toString + "\""
d.toEpochDay.toString
val x = LocalDate.ofEpochDay(-1)

val obj: Map[String, String] = Map(("foo" -> "bar"), ("baz" -> "Bum"))
obj.exists { case (_, value) =>
  value == "Bum"
}

case class Foo(bar: Int)

val xyz = List[Foo](Foo(1), Foo(-1), Foo(2))

xyz.minByOption(_.bar)

val option1 = Option(1)
val option2 = Option(1)
val option3 = Option.empty[Int]
val option4 = Option.empty[Int]

val abc = (option1, option2)

val map1 = Map(1 -> "C/C++", 5 -> "foobar")
val map2 = Map(5 -> "Java", 8 -> "Scala")

println("Map1 : " + map1)
println("Map2 : " + map2)

// concatenating maps
val concMap = map1.++(map2)
println("Concatenated Map : " + concMap)

val firstInts  = List(1, 2, 3)
val secondInts = List(3, 4, 5, 6)
val thirdInts  = List(7, 8)

val zipIntResult = for {
  firstInt  <- firstInts
  secondInt <- secondInts
  thirdInt  <- thirdInts
} yield (firstInt, secondInt, thirdInt)

def simpleArraySum(ar: Array[Int]): Int =
  ar.foldLeft(0)(_ + _)

val l1 = List("apple")
val l2 = List("apple", "apple")

l1.diff(l2)
l2.diff(l1)

val INVALID_CHARACTERS = List('!', '~', '}')
val candidateString    = "fo!obar"

val ttyl = "a"
val qrs = List("b", "c")

qrs :+ ttyl


Duration.between(Instant.now().plusSeconds(30), Instant.now())