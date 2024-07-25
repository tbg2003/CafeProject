package Utils

trait POSError {def message:String}

object POSError{
  case class InvalidItemType(message:String) extends POSError
  case class InvalidStamp(message:String) extends POSError
  case class InsufficientStamps(message:String) extends POSError
  case class InvalidAge(message:String) extends POSError
  case class AlreadyHasCard(message:String) extends POSError
  case class InvalidMinPurchases(message:String) extends POSError
  case class InvalidMinSpendTotal(message:String) extends POSError
  case class InvalidRemovingCard(message:String) extends POSError
}
