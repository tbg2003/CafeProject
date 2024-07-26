package LoyaltyCard

import Error.POSError
import Error.POSError.InvalidStamp

import java.time.LocalDate

case class DiscountLoyaltyCard(customerStars: Option[List[LocalDate]]) extends LoyaltyCard {
  val maxStars: Int = 8
  private var currentCustomerStars = customerStars match {
    case Some(stars) => if (stars.nonEmpty && stars.length <= maxStars) stars else List()
    case None => List()
  }

  def getCustomersCard(): List[LocalDate] = {
    currentCustomerStars
  }


  def addStar(orderPrice: Double): Either[POSError, Option[List[LocalDate]]] = {
    if (orderPrice >= 20) {
      if (currentCustomerStars.contains(LocalDate.now)) {
        Left(InvalidStamp("Stars Already Added Stamped today"))
      }
      else if (currentCustomerStars.length >= maxStars) Left(InvalidStamp("No more stars can be added"))
      else {
        currentCustomerStars = currentCustomerStars :+ LocalDate.now()
        Right(Some(currentCustomerStars))
      }
    }
    else {
      Left(InvalidStamp("Purchase price is less than 20 pounds"))
    }

  }

  def getDiscount(): Double = {
    currentCustomerStars.length * 0.02
  }

}
