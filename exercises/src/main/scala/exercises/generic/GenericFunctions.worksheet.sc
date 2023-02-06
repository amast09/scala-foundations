import java.time.LocalDate
import exercises.generic.GenericFunctionExercises._

println("Hello world")
println(secret.map(_.map(_.toChar).reverse.mkString).swap)


val replicate: (Int, String) => String = (numReplications, s) => List.fill(numReplications)(s).toString()

replicate(3, "hello ")

val d = LocalDate.now()

"\"" + d.toString + "\""
d.toEpochDay.toString
val x = LocalDate.ofEpochDay(-1)