package Customer

import LoyaltyCard.{LoyaltyCard, LoyaltyCardType}
import Utils.POSError

case class Customer(fullName: String, age:Int) {

  private var totalSpent: Double = 0
  private var totalPurchases: Int = 0

  def getTotalSpent(): Double = {
    totalSpent
  }

  def getTotalPurchases(): Int = {
    totalPurchases
  }

  def newOrder(purchaseAmount: Double): Tuple2[Double, Int] = {
    totalSpent += purchaseAmount
    totalPurchases += 1
    Tuple2(totalSpent, totalPurchases)

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







}
