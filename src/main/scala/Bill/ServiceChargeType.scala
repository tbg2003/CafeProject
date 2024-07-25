package Bill

sealed trait ServiceChargeType

object ServiceChargeType{
  case object NoServiceCharge extends ServiceChargeType
  case object HotDrinkServiceCharge extends ServiceChargeType
  case object HotFoodServiceCharge extends ServiceChargeType
  case object SpecialServiceCharge extends ServiceChargeType
}
