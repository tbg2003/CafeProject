package Utils

sealed class CurrencyType(val conversionRate:Double){
  def convertTo(billTotal:Double):Double = {
    billTotal * conversionRate

  }
}

object CurrencyType{
  case object GBP extends CurrencyType(1.0)
  case object EUR extends CurrencyType(1.19)
  case object USD extends CurrencyType(1.29)
  case object INR extends CurrencyType(107.73)
  case object AUD extends CurrencyType(1.96)
  case object YEN extends CurrencyType(198.54)
  case object NOK extends CurrencyType(14.17)
}