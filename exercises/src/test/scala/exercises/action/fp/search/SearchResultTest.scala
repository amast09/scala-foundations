package exercises.action.fp.search

import org.scalatest.funsuite.AnyFunSuite
import SearchFlightGenerator._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import Ordering.Implicits._

class SearchResultTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("cheapest, fastest and best are consistent with flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult.fromFlights(flights)

      (result.cheapest, result.fastest, result.best) match {
        case (None, None, None)             => assert(result.flights.isEmpty)
        case (Some(f1), Some(f2), Some(f3)) => assert(result.flights.exists(Set(f1, f2, f3)))
        case _                              => fail("inconsistent")
      }
    }
  }

  test("flights all come from the inputs") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult.fromFlights(flights)

      result.flights.foreach { resultFlight =>
        assert(flights.contains(resultFlight))
      }
    }
  }

  test("cheapest is the cheapest flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult.fromFlights(flights)

      for {
        cheapest <- result.cheapest
        flight   <- result.flights
      } assert(cheapest.unitPrice <= flight.unitPrice)
    }
  }

  test("fastest is the fastest flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult.fromFlights(flights)

      for {
        fastest <- result.fastest
        flight  <- result.flights
      } assert(fastest.duration <= flight.duration)
    }
  }

  test("flights are sorted by number of stops and then price") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result   = SearchResult.fromFlights(flights)
      val expected = result.flights.sorted(SearchResult.bestOrdering)
      assert(result.flights == expected)
    }
  }

  ignore("no flights have the same ids") {
    forAll(Gen.listOf(flightGen), MinSuccessful(100)) { (flights: List[Flight]) =>
      val result = SearchResult.fromFlights(flights)

      result.flights.groupBy(_.flightId).foreach {
        case (_, flightsById) => assert(flightsById.size == 1)
      }
    }
  }

}