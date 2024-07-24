package LoyaltyCard

import Utils.POSError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class DrinksLoyaltyCardSpec extends AnyWordSpec with Matchers{
  val date1: LocalDate = LocalDate.of(2024, 5, 1)
  val date2: LocalDate = LocalDate.of(2024, 5, 2)
  val date3: LocalDate = LocalDate.of(2024, 5, 3)
  val date4: LocalDate = LocalDate.of(2024, 5, 4)
  val date5: LocalDate = LocalDate.of(2024, 5, 5)
  val date6: LocalDate = LocalDate.of(2024, 5, 6)
  val date7: LocalDate = LocalDate.of(2024, 5, 7)
  val date8: LocalDate = LocalDate.of(2024, 5, 8)
  val date9: LocalDate = LocalDate.of(2024, 5, 9)
  val today: LocalDate = LocalDate.now()

  val cardWith9Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9)
  "addStamp" should{
    "Return a Right" when{
      "Adding stamp on a new day" in{
        val cardOldDates:List[LocalDate] = List(date1, date2, date3, date4, date5)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardOldDates))
        loyaltyCard.addStamp() shouldBe Right(cardOldDates:+LocalDate.now())
      }
    }
    "Return a Left" when{
      "Already stamped today" in{
        val cardTodaysDate:List[LocalDate] = List(date1, date2, date3, date4, date5, today)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardTodaysDate))
        loyaltyCard.addStamp() shouldBe Left(POSError.InvalidStamp("Already received a stamp today"))
      }
      "AStamped twice one day" in{
        val cardTodaysDate:List[LocalDate] = List(date1, date2, date3, date4, date5)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardTodaysDate))
        loyaltyCard.addStamp()
        loyaltyCard.addStamp() shouldBe Left(POSError.InvalidStamp("Already received a stamp today"))
      }
    }
  }
  "checkEnoughStamps" should {
  }
  "getFreeDrink"
}
