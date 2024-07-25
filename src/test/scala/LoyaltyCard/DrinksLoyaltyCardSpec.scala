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
  val date10: LocalDate = LocalDate.of(2024, 5, 10)
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
        val cardWithStamps:List[LocalDate] = List(date1, date2, date3, date4, date5)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWithStamps))
        loyaltyCard.addStamp()
        loyaltyCard.addStamp() shouldBe Left(POSError.InvalidStamp("Already received a stamp today"))
      }
    }
  }
  "checkEnoughStamps" should {
    "Return a Right" when {
      "Loyalty Card has 10 stamps" in {
        val cardWith10Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9, date10)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith10Stamps))
        loyaltyCard.checkEnoughStamps() shouldBe Right(true)
      }
    }
    "Return a Left" when {
      "Loyalty card has less than 10 stamps" in {
        val cardWithout10Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWithout10Stamps))
        loyaltyCard.checkEnoughStamps() shouldBe Left(POSError.InsufficientStamps(s"Not enough stamps, you need 10"))
      }
    }

  }
  "getFreeDrink" should {
    "return Left" when {
      "stamp has already been stamped today" in {
        val cardTodaysDate:List[LocalDate] = List(date1, date2, date3, date4, date5, today)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardTodaysDate))
        loyaltyCard.getFreeDrink() shouldBe Left(POSError.InvalidStamp("Already received a stamp today"))
      }
      "Loyalty card has less than 10 stamps" in {
        val cardWithStamps:List[LocalDate] = List(date1, date2, date3, date4, date5)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWithStamps))
        loyaltyCard.checkEnoughStamps() shouldBe Left(POSError.InsufficientStamps(s"Not enough stamps, you need 10"))
      }
    }
    "return a Left" when {
      "Card has not been stamped today and currently has 9 stamps" in {
        val cardWith9Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9)
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith9Stamps))
        loyaltyCard.getFreeDrink() shouldBe Right(true)
      }
    }
  }
}
