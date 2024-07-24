package LoyaltyCard

import Utils.POSError

import java.time.LocalDate

case class DrinksLoyaltyCard(customerStamps:Option[List[LocalDate]]) extends LoyaltyCard{


  private var stamps:List[LocalDate] = List()

  def addStamp():Either[POSError, List[LocalDate]] = {
    val now:LocalDate = LocalDate.now()
    if (stamps.contains(now))Left(POSError.InvalidStamp("Already received a stamp today"))
    else Right(stamps :+ now)
  }
  def checkEnoughStamps():Either[POSError, Boolean] ={
    if(stamps.length % 10 == 0) Right(true)
    else Left(POSError.InsufficientStamps(s"Not enough stamps, you need 10"))
  }

  def getFreeDrink():Either[POSError, Boolean] = {
    for{
      _ <- addStamp()
      _ <- checkEnoughStamps()
    } yield true
  }
}
