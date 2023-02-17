package exercises.errorhandling.project

object OrderStatuses {
  val Draft = "Draft"
  val Checkout = "Checkout"
  val Submitted = "Submitted"
  val Delivered = "Delivered"
}

sealed trait OrderStatus
object OrderStatus {
  case object Draft extends OrderStatus
  case object Checkout extends OrderStatus
  case object Submitted extends OrderStatus
  case object Delivered extends OrderStatus
}
