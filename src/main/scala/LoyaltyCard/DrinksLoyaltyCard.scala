package LoyaltyCard

import Utils.POSError

import java.time.LocalDate

case class DrinksLoyaltyCard(customerStamps:Option[List[LocalDate]]) extends LoyaltyCard{

  private var cardStamps:List[LocalDate] = customerStamps match {
    case Some(stampList) => stampList
    case None => List()
  }

  def addStamp():Either[POSError, List[LocalDate]] = {
    val now:LocalDate = LocalDate.now()
    if (cardStamps.contains(now))Left(POSError.InvalidStamp("Already received a stamp today"))
    else {
      cardStamps = cardStamps :+ now
      Right(cardStamps)
    }
  }
  def checkEnoughStamps():Either[POSError, Boolean] ={
    if(cardStamps.length % 10 == 0) Right(true)
    else Left(POSError.InsufficientStamps(s"Not enough stamps, you need 10"))
  }

  // ONLY CALL THIS ONE
  def getFreeDrink():Either[POSError, Boolean] = {
    for{
      _ <- addStamp()
      _ <- checkEnoughStamps()
    } yield true
  }
}
