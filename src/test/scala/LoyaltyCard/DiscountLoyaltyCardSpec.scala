package LoyaltyCard

import LoyaltyCard.DiscountLoyaltyCard
import Error.POSError.InvalidStamp
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class DiscountLoyaltyCardSpec extends AnyWordSpec with Matchers {
  // Empty List
  val date1: LocalDate = LocalDate.of(2024, 5, 1)
  val date2: LocalDate = LocalDate.of(2024, 3, 2)
  val date3: LocalDate = LocalDate.of(2024, 5, 3)
  val date4: LocalDate = LocalDate.of(2024, 6, 4)
  val date5: LocalDate = LocalDate.of(2024, 5, 5)
  val date6: LocalDate = LocalDate.of(2024, 8, 6)
  val date7: LocalDate = LocalDate.of(2024, 5, 7)
  val date8: LocalDate = LocalDate.of(2024, 7, 8)
  val date9: LocalDate = LocalDate.of(2024, 3, 9)
  val date10: LocalDate = LocalDate.of(2024, 5, 10)
  val today: LocalDate = LocalDate.now()


  val emptyStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List()))
  val fourStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4)))
  val twoStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2)))
  val fiveStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5)))
  val sixStampDiscountLoyaltyCardWithTodayDate: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5, today)))
  val eightStampedDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5, date6, date7, date8)))
  val todayStampedDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5, today)))





  "getCustomersStarts " should {
    "Return an empty List" when {
      "the list length is negative  " in {
        val emptyDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard.apply(Some(List()))
        emptyDiscountLoyaltyCard.getCustomersCard() shouldBe List()
      }
    }
    "Return an None List" when {
      "the customer stars is None " in {
        val noneParameterDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard.apply(None)
        noneParameterDiscountLoyaltyCard.getCustomersCard() shouldBe List()
      }
    }
    "Return an Non-Empty List" when {
      "the the object is instantiated with non empty dates " in {
        val cardWithThreeDates: List[LocalDate] = List(date1, date2, date3)
        val noneEmptyDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(cardWithThreeDates))
        noneEmptyDiscountLoyaltyCard.getCustomersCard() shouldBe (cardWithThreeDates)


      }
    }
  }

  "addStar" should {
    "return Left" when {
      "the order price is less than 20" in {
        fourStampDiscountLoyaltyCard.addStar(19) shouldBe(Left(InvalidStamp("Purchase price is less than 20 pounds")))
      }
    }
    "return Left" when {
      "the stamp has today date" in {
        todayStampedDiscountLoyaltyCard.addStar(20) shouldBe(Left(InvalidStamp("Stars Already Added Stamped today")))
      }
    }
    "return Left" when {
      "the stamp length is 8" in {
      eightStampedDiscountLoyaltyCard.addStar(100) shouldBe(Left(InvalidStamp("No more stars can be added")))
      }
    }
    "return Right" when {
      "the stamp length is less than 8 and not stamped today" in {
        fiveStampDiscountLoyaltyCard.addStar(100) shouldBe Right((sixStampDiscountLoyaltyCardWithTodayDate.customerStars))
      }
    }
  }

  "getDiscount" should {
    "return 0 when 0 stars on card" in {
      emptyStampDiscountLoyaltyCard.getDiscount() shouldBe 0
    }
    "return 0.04 when 2 stars on card" in {
      twoStampDiscountLoyaltyCard.getDiscount() shouldBe 0.04
    }
    "return 0.08 when 4 stars on card" in {
      fourStampDiscountLoyaltyCard.getDiscount() shouldBe 0.08
    }
    "return 0.16 when 8 stars on card" in {
      eightStampedDiscountLoyaltyCard.getDiscount() shouldBe 0.16
    }
  }

}
