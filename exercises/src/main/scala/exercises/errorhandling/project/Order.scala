package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatuses._

import java.time.{Duration, Instant}
import exercises.errorhandling.NEL

sealed trait RedefinedOrder {
  val id: RedefinedOrder.OrderId
  val createdAt: Instant
  val orderStatus: OrderStatus
}

sealed trait OpenOrder extends RedefinedOrder {
  def addItem(item: RedefinedOrder.Item): RedefinedOrder.DraftWithBasket =
    this.addItems(NEL(item))

  def addItems(items: NEL[RedefinedOrder.Item]): RedefinedOrder.DraftWithBasket =
    RedefinedOrder.DraftWithBasket(this.id, this.createdAt, items)
}

sealed trait CheckoutOrder extends OpenOrder {
  val basket: NEL[RedefinedOrder.Item]

  def updateDeliveryAddress(deliveryAddress: Address): RedefinedOrder.CheckoutWithAddress =
    RedefinedOrder.CheckoutWithAddress(this.id, this.createdAt, this.basket, deliveryAddress)
}

object RedefinedOrder {
  case class OrderId(value: String)
  case class ItemId(value: String)
  case class Item(id: ItemId, quantity: Int, unitPrice: Double)

  case class DraftNoBasket(id: OrderId, createdAt: Instant) extends OpenOrder {
    val orderStatus = OrderStatus.Draft
  }

  case class DraftWithBasket(id: OrderId, createdAt: Instant, basket: NEL[RedefinedOrder.Item]) extends OpenOrder {
    val orderStatus = OrderStatus.Draft

    def checkout(): CheckoutNoAddress =
      CheckoutNoAddress(this.id, this.createdAt, this.basket)
  }

  case class CheckoutNoAddress(id: OrderId, createdAt: Instant, basket: NEL[RedefinedOrder.Item])
      extends CheckoutOrder {
    val orderStatus = OrderStatus.Checkout
  }

  case class CheckoutWithAddress(
    id: OrderId,
    createdAt: Instant,
    basket: NEL[RedefinedOrder.Item],
    deliveryAddress: Address
  ) extends CheckoutOrder {
    val orderStatus = OrderStatus.Checkout

    def submit(submittedAt: Instant): Submitted =
      Submitted(this.id, this.createdAt, this.basket, this.deliveryAddress, submittedAt)
  }

  case class Submitted(
    id: OrderId,
    createdAt: Instant,
    basket: NEL[RedefinedOrder.Item],
    deliveryAddress: Address,
    submittedAt: Instant
  ) extends RedefinedOrder {
    val orderStatus = OrderStatus.Submitted

    def deliver(deliveredAt: Instant): Delivered =
      Delivered(this.id, this.createdAt, this.basket, this.deliveryAddress, this.submittedAt, deliveredAt)
  }

  case class Delivered(
    id: OrderId,
    createdAt: Instant,
    basket: NEL[Item],
    deliveryAddress: Address,
    submittedAt: Instant,
    deliveredAt: Instant
  ) extends RedefinedOrder {
    val orderStatus                = OrderStatus.Delivered
    val shippingDuration: Duration = Duration.between(submittedAt, deliveredAt)
  }
}

case class Order(
  id: String,
  status: String,                   // "Draft", "Checkout", "Submitted" or "Delivered"
  basket: List[Item],               // basket can be modified only in "Draft" or "Checkout"
  deliveryAddress: Option[Address], // can only be set during "Checkout"
  createdAt: Instant,               // set when the order is created ("Draft")
  submittedAt: Option[Instant],     // set when the order is moved to "Submitted"
  deliveredAt: Option[Instant]      // set when the order is moved to "Delivered"
) {

  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): Either[OrderError, Order] =
    status match {
      case Draft | Checkout => Right(copy(status = Draft, basket = basket :+ item))
      case _                => Left(InvalidStatus(status))
    }

  // 1. Implement `checkout` which attempts to move the `Order` to "Checkout" status.
  // `checkout` requires the order to be in the "Draft" status, otherwise it returns an `InvalidStatus` error.
  // `checkout` requires the order to contain at least one item, otherwise it returns an `EmptyBasket` error.
  def checkout: Either[OrderError, Order] = this match {
    case Order(_, Draft, _ :: _, _, _, _, _) =>
      Right(this.copy(status = Checkout))
    case Order(_, Draft, List(), _, _, _, _) =>
      Left(OrderError.EmptyBasket)
    case o: Order =>
      Left(OrderError.InvalidStatus(o.status))
  }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case Checkout => Right(copy(deliveryAddress = Some(address)))
      case _        => Left(InvalidStatus(status))
    }

  // 2. Implement `submit` which attempts to move the `Order` to "Submitted" status.
  // `submit` requires the order to be in the "Checkout" status and to have a delivery address.
  // If `submit` succeeds, the resulting order must be in "Submitted" status and
  // have the field `submittedAt` defined.
  // Note: You may need to extend `OrderError`
  def submit(now: Instant): Either[OrderError, Order] = this match {
    case Order(_, Checkout, _ :: _, Some(_), _, _, _) =>
      Right(this.copy(status = Submitted, submittedAt = Some(now)))
    case Order(_, Checkout, _ :: _, None, _, _, _) =>
      Left(OrderError.MissingDeliveryAddress)
    case Order(_, Checkout, List(), Some(_), _, _, _) =>
      Left(OrderError.EmptyBasket)
    case o: Order =>
      Left(OrderError.InvalidStatus(o.status))
  }

  // 3. Implement `deliver` which attempts to move the `Order` to "Delivered" status.
  // `deliver` requires the order to be in the "Submitted" status.
  // If `deliver` succeeds, the resulting order must be in "Delivered" status and
  // have the field `deliveredAt` defined.
  // If `deliver` succeeds, it also returns the time it took to deliver the order (duration
  // between `submittedAt` and `deliveredAt`).
  // Note: You may need to extend `OrderError`
  def deliver(now: Instant): Either[OrderError, (Order, Duration)] =
    this match {
      case Order(_, Submitted, _ :: _, Some(_), _, Some(submittedAt), _) =>
        val deliveredAt      = now
        val shippingDuration = Duration.between(submittedAt, deliveredAt)

        Right(
          (
            this.copy(status = Delivered, deliveredAt = Some(deliveredAt)),
            shippingDuration
          )
        )
      case Order(_, Submitted, _ :: _, Some(_), _, None, _) =>
        Left(OrderError.MissingSubmittedAt)
      case Order(_, Submitted, _ :: _, None, _, Some(_), _) =>
        Left(OrderError.MissingDeliveryAddress)
      case Order(_, Submitted, List(), Some(_), _, Some(_), _) =>
        Left(OrderError.EmptyBasket)
      case o: Order =>
        Left(OrderError.InvalidStatus(o.status))
    }
}

object Order {
  // Creates an empty draft order.
  def empty(id: String, now: Instant): Order =
    Order(
      id = id,
      status = Draft,
      basket = Nil,
      deliveryAddress = None,
      createdAt = now,
      submittedAt = None,
      deliveredAt = None
    )
}
