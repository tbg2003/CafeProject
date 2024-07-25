package LoyaltyCard

import Utils.POSError
import Utils.POSError.InvalidStamp

import java.time.LocalDate

case class DiscountLoyaltyCard(customerStars: Option[List[LocalDate]]) extends LoyaltyCard {
  val maxStars: Int = 8
  private val currentCustomerStars = customerStars match {
    case Some(stars) => if (stars.nonEmpty && stars.length<=maxStars) stars else List()
    case None => List()
  }

  def getCustomersCard(): List[LocalDate] ={
    currentCustomerStars
  }


  def addStar(orderPrice: Double): Either[POSError, String] = {
    if (orderPrice >20){
      if(customerStars.contains(Some(LocalDate.now))){
        Left(InvalidStamp("Stars Already Added"))
      }
      else if (currentCustomerStars.length >= maxStars) Left(InvalidStamp("No more stars can be added"))
      else{
        currentCustomerStars:+ LocalDate.now()
        Right("One star Added Successfully!!!")
      }
    }
    else{
      Left(InvalidStamp("Purchase price is less than 20 pounds"))
    }

  }

  def getDiscount(): Double = {
    currentCustomerStars.length*0.02
  }

}
