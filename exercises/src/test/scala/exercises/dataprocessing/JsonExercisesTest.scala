package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import exercises.dataprocessing.JsonExercises._

class JsonExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val john: Json = JsonObject(
    Map(
      "name" -> JsonString(" John Doe "),
      "age"  -> JsonNumber(25),
      "kids" -> JsonArray(List(JsonString(" Jeremy"), JsonString("Molly "))),
      "address" -> JsonObject(
        Map(
          "street-number" -> JsonNumber(25),
          "street-name"   -> JsonString("  Cody Road")
        )
      )
    )
  )
  val jane: Json = JsonObject(
    Map(
      "name" -> JsonString(" John Doe "),
      "age" -> JsonObject(
        Map(
          "age-number" -> JsonNumber(232),
          "age-name"   -> JsonString("two hundred thirty two")
        )
      ),
      "address" -> JsonObject(
        Map(
          "street" -> JsonObject(
            Map(
              "number" -> JsonNumber(23),
              "owner" -> JsonArray(
                List(
                  JsonNumber(1991),
                  JsonObject(
                    Map(
                      "age-number" -> JsonNumber(232),
                      "age-name"   -> JsonString("nested-string")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  val doe: Json = JsonObject(
    Map(
      "name" -> JsonString(" John Doe "),
      "age" -> JsonObject(
        Map(
          "age-number" -> JsonNumber(232),
          "age-name"   -> JsonString("two hundred thirty two")
        )
      ),
      "address" -> JsonObject(
        Map(
          "street" -> JsonObject(
            Map(
              "number" -> JsonNumber(23),
              "owner"  -> JsonString("Cody Keim")
            )
          )
        )
      )
    )
  )

  test("trimAll") {
    assert(
      trimAll(john) == JsonObject(
        Map(
          "name" -> JsonString("John Doe"),
          "age"  -> JsonNumber(25),
          "kids" -> JsonArray(List(JsonString("Jeremy"), JsonString("Molly"))),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(25),
              "street-name"   -> JsonString("Cody Road")
            )
          )
        )
      )
    )
  }

  test("anonymize") {
    assert(
      anonymize(john) == JsonObject(
        Map(
          "name" -> JsonString("***"),
          "age"  -> JsonNumber(0),
          "kids" -> JsonArray(List(JsonString("***"), JsonString("***"))),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(0),
              "street-name"   -> JsonString("***")
            )
          )
        )
      )
    )
  }

  test("search") {
    assert(search(JsonObject(Map.empty), "ll") == false)
    assert(search(JsonNumber(5), "ll") == false)
    assert(search(JsonString("Hello"), "ll") == true)
    assert(search(john, "Jeremy") == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ll") == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ss") == false)
    assert(search(JsonObject(Map("message" -> JsonString("hi"))), "ll") == false)
  }

  test("depth") {
    assert(depth(JsonNumber(1)) == 0)
    assert(depth(JsonObject(Map.empty)) == 0)
    assert(depth(JsonObject(Map("k" -> JsonNumber(1)))) == 1)
    assert(depth(john) == 2)
    assert(depth(doe) == 3)
    assert(depth(jane) == 4)
  }

  test("search max depth") {
    assert(searchDepth(JsonObject(Map.empty), "ll", 100) == false)
    assert(searchDepth(JsonNumber(5), "ll", 100) == false)
    assert(searchDepth(JsonString("Hello"), "ll", 100) == true)
    assert(searchDepth(john, "Jeremy", 100) == true)
    assert(searchDepth(JsonObject(Map("message" -> JsonString("Hello"))), "ll", 100) == true)
    assert(searchDepth(JsonObject(Map("message" -> JsonString("Hello"))), "ss", 100) == false)
    assert(searchDepth(JsonObject(Map("message" -> JsonString("hi"))), "ll", 100) == false)
    assert(searchDepth(jane, "nested-string", 3) == false)
    assert(searchDepth(jane, "nested-string", 4) == false)
  }

}
