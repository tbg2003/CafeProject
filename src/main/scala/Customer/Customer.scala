package Customer

import LoyaltyCard.{LoyaltyCard, LoyaltyCardType}
import Utils.POSError

case class Customer(id: Int, fullName: String, age:Int, totalSpent: Double = 0, totalPurchases: Int = 0) {

  private var currentTotalSpent: Double = totalSpent
  private var currentTotalPurchases: Int = totalPurchases
  private var currentLoyaltyCard: Option[LoyaltyCard] = None

  def getTotalSpent(): Double = {
    currentTotalSpent
  }

  def getTotalPurchases(): Int = {
    currentTotalPurchases
  }

  def newOrder(purchaseAmount: Double): (Double, Int) = {
    currentTotalSpent += purchaseAmount
    currentTotalPurchases += 1
    Tuple2(currentTotalSpent, currentTotalPurchases)

  }

  def applyForLoyaltyCard(loyaltyCard: LoyaltyCardType): Either[POSError, LoyaltyCard] = {
    ???
  }

  def applyForDrinksLoyaltyCard(loyaltyCard: LoyaltyCardType): Either[POSError, LoyaltyCard] = {
    ???
  }

  def applyForDiscountLoyaltyCard(loyaltyCard: LoyaltyCardType): Either[POSError, LoyaltyCard] = {
    ???
  }

  def isValidAge(): Either[POSError, Boolean] = {
    ???
  }

  def hasLoyaltyCard(): Either[POSError, Boolean] = {
    ???
  }

  def hasMadeMinPurchases(): Either[POSError, Boolean] = {
    ???
  }

  def hasSpentMinTotal(): Either[POSError, Boolean] = {
    ???
  }

  def removeCurrentLoyaltyCard():Either[POSError, Boolean] = {
    ???
  }







}
