package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus}
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatuses._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}
import exercises.errorhandling.NEL

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = Order(
      id = "AAA",
      status = Draft,
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout)
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = Draft,
      basket = Nil,
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val order = Order(
      id = "AAA",
      status = Delivered,
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(InvalidStatus(Delivered)))
  }

  test("submit successful example") {
    val order = Order(
      id = "AAA",
      status = Checkout,
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.submit(Instant.now()) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted)
    }
  }

  test("submit no address example") {
    val order = Order(
      id = "AAA",
      status = Checkout,
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(OrderError.MissingDeliveryAddress))
  }

  test("submit invalid status example") {
    val order = Order(
      id = "AAA",
      status = Delivered,
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus(Delivered)))
  }

  test("submit empty basket example") {
    val order = Order(
      id = "AAA",
      status = Checkout,
      basket = Nil,
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(EmptyBasket))
  }

  test("happy path") {
    forAll(arbitrary[String], instantGen, addressGen, Gen.nonEmptyListOf(itemGen), durationGen, durationGen) {
      (
        orderId: String,
        createdAt: Instant,
        deliveryAddress: Address,
        items: List[Item],
        submittedAtDelay: Duration,
        deliveredAtDelay: Duration
      ) =>
        val order       = Order.empty(orderId, createdAt)
        val submittedAt = createdAt.plus(submittedAtDelay)
        val deliveredAt = submittedAt.plus(deliveredAtDelay)
        val orderAfterItemsAdded = items.foldLeft[Either[OrderError, Order]](Right(order)) {
          (maybeLastOrder, itemToAdd) =>
            maybeLastOrder.flatMap(lastOrder => lastOrder.addItem(itemToAdd))
        }

        val result = for {
          order         <- orderAfterItemsAdded
          order         <- order.checkout
          order         <- order.updateDeliveryAddress(deliveryAddress)
          order         <- order.submit(submittedAt)
          orderDuration <- order.deliver(deliveredAt)
        } yield orderDuration

        assert(
          result.map(_._1) == Right(
            Order(
              id = orderId,
              status = Delivered,
              basket = items,
              deliveryAddress = Some(deliveryAddress),
              createdAt = createdAt,
              submittedAt = Some(submittedAt),
              deliveredAt = Some(deliveredAt)
            )
          )
        )

        assert(result.map(_._2) == Right(Duration.between(submittedAt, deliveredAt)))
    }
  }

  test("RedefinedOrder happy path") {
    forAll(arbitrary[String], instantGen, addressGen, nelOf(itemGen), durationGen, durationGen) {
      (
        orderId: String,
        createdAt: Instant,
        deliveryAddress: Address,
        items: NEL[Item],
        submittedAtDelay: Duration,
        deliveredAtDelay: Duration
      ) =>
        val submittedAt    = createdAt.plus(submittedAtDelay)
        val deliveredAt    = submittedAt.plus(deliveredAtDelay)
        val redefinedItems = items.map(i => RedefinedOrder.Item(RedefinedOrder.ItemId(i.id), i.quantity, i.unitPrice))

        val expectedOrder = RedefinedOrder.Delivered(
          id = RedefinedOrder.OrderId(orderId),
          basket = redefinedItems,
          deliveryAddress = deliveryAddress,
          createdAt = createdAt,
          submittedAt = submittedAt,
          deliveredAt = deliveredAt
        )

        val emptyOrder       = RedefinedOrder.DraftNoBasket(RedefinedOrder.OrderId(orderId), createdAt)
        val orderWithBasket  = emptyOrder.addItems(redefinedItems)
        val orderAtCheckout  = orderWithBasket.checkout()
        val orderWithAddress = orderAtCheckout.updateDeliveryAddress(deliveryAddress)
        val submittedOrder   = orderWithAddress.submit(submittedAt)
        val deliveredOrder   = submittedOrder.deliver(deliveredAt)

        assert(deliveredOrder == expectedOrder)

        assert(expectedOrder.shippingDuration == Duration.between(submittedAt, deliveredAt))
    }
  }
}
