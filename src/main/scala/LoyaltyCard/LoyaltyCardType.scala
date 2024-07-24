package LoyaltyCard

sealed trait LoyaltyCardType
object LoyaltyCardType {
  case object DrinksLoyalty extends LoyaltyCardType
  case object DiscountLoyalty extends LoyaltyCardType
}
