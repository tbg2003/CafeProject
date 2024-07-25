package LoyaltyCard

import LoyaltyCard.DiscountLoyaltyCard
import Utils.POSError.InvalidStamp
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

  val fourStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4)))
  val fiveStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5)))
  val discountLoyaltyCardWithTodayDate: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4, date5, today)))


  "getCustomersStarts " should {
    "Return an empty List" when {
      "the list length is negative  " in {
        val emptyDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard.apply(Some(List()))
        emptyDiscountLoyaltyCard.getCustomersCard() shouldBe (Some(List()))
      }
    }
    "Return an None List" when {
      "the customer stars is None " in {
        val noneParameterDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard.apply(None)
        noneParameterDiscountLoyaltyCard.getCustomersCard() shouldBe (None)
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
        discountLoyaltyCardWithTodayDate.addStar(35) shouldBe(Left(InvalidStamp("Stars Already Added")))
      }
    }
  }

}
