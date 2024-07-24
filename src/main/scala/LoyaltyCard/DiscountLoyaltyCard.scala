package LoyaltyCard

import Utils.POSError
import Utils.POSError.InvalidStamp

import java.time.LocalDate
import scala.collection.View.Empty

case class DiscountLoyaltyCard(customerStars: Option[List[LocalDate]]) extends LoyaltyCard {
  val maxStars: Int = 8
  private var currentCustomerStars = customerStars match {
    case Some(stars) => if (stars.length <= 8) stars else List()
    case None => List()
  }


  def addStar(orderPrice: Double): Either[POSError, Int] = {
    if (orderPrice > 20) {
      if (currentCustomerStars.length == 8) Left(InvalidStamp("Star Cannot be added"))
      else if (currentCustomerStars.length < 8) Right()
    }

  }

  def getDiscount(orderPrice: Double): Double = {
    ???
  }

}
