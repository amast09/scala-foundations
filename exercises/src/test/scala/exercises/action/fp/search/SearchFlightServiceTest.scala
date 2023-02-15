package exercises.action.fp.search

import exercises.action.DateGenerator._
import exercises.action.fp.IO
import exercises.action.fp.search.Airport._
import exercises.action.fp.search.SearchFlightGenerator._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.concurrent.ExecutionContext
import scala.util.Random

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  implicit val ec = ExecutionContext.global

  test("fromTwoClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.fromTwoClients(client1, client2)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromTwoClients returns the results of the succeeding client") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight2 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO.fail(new Error("BANG!")))
    val client2 = SearchFlightClient.constant(IO(List(flight1, flight2)))

    val service = SearchFlightService.fromTwoClients(client1, client2)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2)))
  }

  test("fromTwoClients handles failures gracefully") {
    forAll(airportGen, airportGen, dateGen, clientGen, clientGen) {
      (
        departureAirport: Airport,
        arrivalAirport: Airport,
        departureTime: LocalDate,
        client1: SearchFlightClient,
        client2: SearchFlightClient
      ) =>
        val service = SearchFlightService.fromTwoClients(client1, client2)

        val result = service.search(departureAirport, arrivalAirport, departureTime).attempt.unsafeRun()

        assert(result.isSuccess)
    }
  }

  test("fromClients aggregates the results of the clients") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val expectedFlights = List(
      Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, ""),
      Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, ""),
      Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, ""),
      Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")
    )
    val clients = expectedFlights.map(f => SearchFlightClient.constant(IO(List(f))))

    val service = SearchFlightService.fromClients(clients)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(expectedFlights))
  }

  test("fromClients handles failures gracefully") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) {
      (
        departureAirport: Airport,
        arrivalAirport: Airport,
        departureTime: LocalDate,
        clients: List[SearchFlightClient]
      ) =>
        val service = SearchFlightService.fromClients(clients)

        val result = service.search(departureAirport, arrivalAirport, departureTime).attempt.unsafeRun()

        assert(result.isSuccess)
    }
  }

  test("fromClients is client order agnostic") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) {
      (
        departureAirport: Airport,
        arrivalAirport: Airport,
        departureTime: LocalDate,
        clients: List[SearchFlightClient]
      ) =>
        val service1 = SearchFlightService.fromClients(clients)
        val service2 = SearchFlightService.fromClients(Random.shuffle(clients))

        val result1 = service1.search(departureAirport, arrivalAirport, departureTime).attempt.unsafeRun()
        val result2 = service2.search(departureAirport, arrivalAirport, departureTime).attempt.unsafeRun()

        assert(result1 == result2)
    }
  }

}
