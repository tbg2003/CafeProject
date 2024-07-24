
trait POSError {def message:String}

object POSError{
  case class InvalidItemType(message:String) extends POSError
}
