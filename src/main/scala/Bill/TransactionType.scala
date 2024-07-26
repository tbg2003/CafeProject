package Bill


sealed trait TransactionType
object TransactionType{
  case object Cash extends TransactionType
  case object Card extends TransactionType
  case object Amex extends TransactionType
}