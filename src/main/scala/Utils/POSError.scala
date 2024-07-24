package Utils

trait POSError {def message:String}

object POSError{
  case class InvalidItemType(message:String) extends POSError
  case class InvalidStamp(message:String) extends POSError
  case class InsufficientStamps(message:String) extends POSError
}
