package LoyaltyCard

import Utils.POSError

import java.time.LocalDate

case class DiscountLoyaltyCard(customerStars:Option[Int], customerStamps:Option[List[LocalDate]]) extends LoyaltyCard {
  val maxStars:Int = 8

  def addStar(orderPrice:Double):Either[POSError, Int] = {
    ???
  }
  def getDiscount(orderPrice:Double):Double = {}
  ???
}
