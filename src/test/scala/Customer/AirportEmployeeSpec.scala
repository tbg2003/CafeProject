package Customer

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}
import Utils.POSError.InvalidServicePeriod
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class AirportEmployeeSpec extends AnyWordSpec with Matchers {
  implicit val invalidAge: Int = 17
  implicit val validAge: Int = 19
  implicit val exactly18: Int = 18
  implicit val invalidTotalSpend: Double = 100.00
  implicit val validTotalSpend: Double = 160.00
  implicit val validTotalSpend150: Double = 150.00
  implicit val totalPurchasesUnder5: Int = 2
  implicit val totalPurchasesOver5: Int = 8
  implicit val emptyStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List()))
  implicit val emptyDrinksLoyaltyCard: DrinksLoyaltyCard = DrinksLoyaltyCard(Some(List()))


  val lessThanSixMonthsDate: LocalDate = LocalDate.of(2024, 5, 1)
  val moreThanSixMonthsOldDate: LocalDate = LocalDate.of(2023, 1, 2)
  val exactlySixMonthsDate: LocalDate = LocalDate.of(2024, 1, 1)
  val moreThan20YearsOfDate: LocalDate = LocalDate.of(1998, 1, 1)



  val date1: LocalDate = LocalDate.of(2024, 6, 4)
  val date2: LocalDate = LocalDate.of(2024, 5, 23)
  val date3: LocalDate = LocalDate.of(2024, 5, 27)
  val date4: LocalDate = LocalDate.of(2024, 2, 18)
  val date5: LocalDate = LocalDate.of(2024, 6, 7)


  val cardOldDates: List[LocalDate] = List(date1, date2, date3, date4, date5)
  val drinksLoyaltyCard: DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardOldDates))
  implicit val fourStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4)))


  "validateEmployeeTenure" should {
    "return Right" when {
      "employee has worked for more than 6 months" in {
        val discountEligibleEmployee: AirportEmployee = new AirportEmployee(id = 1, fullName = "John Doe", age = 28, totalSpent = 100, totalPurchases = 100, loyaltyCard = None,
          startDate = moreThanSixMonthsOldDate)
        discountEligibleEmployee.validateEmployeeTenure() shouldBe Right(10)

      }
      "employee has worked for exactly six months" in {
        val discountEligibleEmployee: AirportEmployee = new AirportEmployee(id = 1, fullName = "John Doe", age = 28, totalSpent = 100, totalPurchases = 100, loyaltyCard = None,
          startDate = exactlySixMonthsDate)
        discountEligibleEmployee.validateEmployeeTenure() shouldBe Right(10)
      }
      "employee has worked for 20 years" in {
        val discountEligibleEmployee: AirportEmployee = new AirportEmployee(id = 1, fullName = "John Doe", age = 28, totalSpent = 100, totalPurchases = 100, loyaltyCard = None,
          startDate = moreThan20YearsOfDate)
        discountEligibleEmployee.validateEmployeeTenure() shouldBe Right(10)
      }

    }
    "return Left" when{
      "employee has worked for less than 6 months" in {
        val discountNotEligibleEmployee: AirportEmployee = new AirportEmployee(id = 1, fullName = "John Doe", age = 28, totalSpent = 100, totalPurchases = 100, loyaltyCard = None,
          startDate = lessThanSixMonthsDate)
        discountNotEligibleEmployee.validateEmployeeTenure() shouldBe Left(InvalidServicePeriod("Not eligible for airport discount"))
      }
    }

  }


}
