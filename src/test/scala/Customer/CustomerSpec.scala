package Customer


import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard, LoyaltyCardType}
import Error.POSError.{AlreadyHasCard, InvalidAge, InvalidMinPurchases, InvalidMinSpendTotal}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class CustomerSpec extends AnyWordSpec with Matchers {

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


  val date1: LocalDate = LocalDate.of(2024, 5, 1)
  val date2: LocalDate = LocalDate.of(2024, 3, 2)
  val date3: LocalDate = LocalDate.of(2024, 5, 3)
  val date4: LocalDate = LocalDate.of(2024, 6, 4)
  val date5: LocalDate = LocalDate.of(2024, 5, 5)

  val cardOldDates: List[LocalDate] = List(date1, date2, date3, date4, date5)
  val drinksLoyaltyCard: DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardOldDates))
  implicit val fourStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4)))


  //  val customer:Customer = new Customer()

  "getTotalSpent" should {
    "return the current total spent" in {
      val customer: Customer = new Customer(1, "John Doe", 18, totalSpent = 200)
      assert(customer.getTotalSpent() == 200)
    }
  }

  "getTotalPurchases" should {
    "return the current total purchases" in {
      val customer: Customer = new Customer(1, "John Doe", 18, totalPurchases = 10)
      assert(customer.getTotalPurchases() == 10)
    }
  }

  "getLoyaltyCard" should {
    "return None" when {
      " there is no loyalty card assigned" in {
        val customer: Customer = new Customer(1, "John Doe", 18)
        customer.getLoyaltyCard() shouldBe None
      }
    }
    "return discount loyalty card" when {
      "discount loyalty card is assigned" in {
        val customer: Customer = new Customer(1, "John Doe", 18, loyaltyCard = Some(emptyStampDiscountLoyaltyCard))
        customer.getLoyaltyCard() shouldBe Some(emptyStampDiscountLoyaltyCard)
      }
    }
  }

  "setLoyaltyCard" should {
    "return None" when {
      " there is no loyalty card assigned given as input parameter" in {
        val customer: Customer = new Customer(1, "John Doe", 18)
        customer.setLoyaltyCard(None) shouldBe None
      }
    }
    "return discount loyalty card" when {
      "discount loyalty card is given as input parameter" in {
        val customer: Customer = new Customer(1, "John Doe", 18)
        customer.setLoyaltyCard(Some(emptyStampDiscountLoyaltyCard)) shouldBe Some(emptyStampDiscountLoyaltyCard)
      }
    }
    "return discount loyalty card" when {
      "drinks loyalty card is given as input parameter" in {
        val customer: Customer = new Customer(1, "John Doe", 18)
        customer.setLoyaltyCard(Some(emptyDrinksLoyaltyCard)) shouldBe Some(emptyDrinksLoyaltyCard)
      }
    }
  }

  "newOrder" should {
    "update total spent with purchase amount, add 1 to total purchases" in {
      val customer: Customer = new Customer(1, "John Doe", 18)
      assert(customer.newOrder(10.00) == (10.00, 1))
    }
  }

  "hasLoyaltyCard" should {
    "return left" when {
      "when customer has loyalty card" in {
        val customer: Customer = new Customer(1, "John Doe", 18, loyaltyCard = Some(emptyStampDiscountLoyaltyCard))
        customer.hasNoLoyaltyCard() shouldBe Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return right" when {
      "when customer has no loyalty card" in {
        val customer: Customer = new Customer(1, "John Doe", 18)
        customer.hasNoLoyaltyCard() shouldBe Right(true)
      }
    }
  }

  "isValidAge" should {
    "return a Left" when {
      "customer's age doesn't meet valid age requirement" in {
        val customer: Customer = new Customer(1, "John Doe", invalidAge)
        customer.isValidAge() shouldBe Left(InvalidAge("Customer is too young"))
      }
    }
    "return a Right" when {
      "customer's age meets valid age requirement" in {
        val customer: Customer = new Customer(1, "John Doe", validAge)
        customer.isValidAge() shouldBe Right(true)
      }
    }
    "return a Right" when {
      "customer's  is exactly 18" in {
        val customer: Customer = new Customer(1, "John Doe", exactly18)
        customer.isValidAge() shouldBe Right(true)
      }
    }
  }

  "hasMadeMinPurchases" should {
    "return a Left" when {
      "customer's total purchases is under 5" in {
        val customer: Customer = new Customer(1, "John Doe", validAge, validTotalSpend, totalPurchasesUnder5)
        customer.hasMadeMinPurchases() shouldBe Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return a Right" when {
      "customer's total purchases is over 5" in {
        val customer: Customer = new Customer(1, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.hasMadeMinPurchases() shouldBe Right(true)
      }
      "customer's total purchases is 5" in {
        val customer: Customer = new Customer(1, "John Doe", validAge, validTotalSpend, 5)
        customer.hasMadeMinPurchases() shouldBe Right(true)
      }
    }
  }
  "hasSpentMinTotal" should {
    "return a Left" when {
      "customers total purchases is less than 150" in {
        val customer = new Customer(3, "John Doe", validAge, invalidTotalSpend, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Left(InvalidMinSpendTotal("Minimum spent less than 150"))
      }
    }
    "return a Right" when {
      "customers total purchases is greater than 150" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Right(true)
      }
    }
    "return a Right" when {
      "customers total purchases is exactly 150" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Right(true)
      }
    }
  }
  "applyForDrinksLoyaltyCard" should {
    "return Left" when {
      "the age is less than 18" in {
        val customer = new Customer(3, "John Doe", invalidAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForDrinksLoyaltyCard() shouldBe Left(InvalidAge("Customer is too young"))
      }
    }
    "return Left" when {
      "the customer already has  LoyaltyCard" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.applyForDrinksLoyaltyCard() shouldBe Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return Left" when {
      "the minimum purchases is less than 5" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesUnder5)
        customer.applyForDrinksLoyaltyCard() shouldBe Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Right" when {
      "the customer is of valid age, has no loyalty card and has made minimum purchases " in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        //customer.setLoyaltyCard()
        customer.applyForDrinksLoyaltyCard() shouldBe Right(DrinksLoyaltyCard(None))
      }
    }
  }


  "applyForDiscountLoyaltyCard" should {
    "return Left" when {
      "the age is less than 18" in {
        val customer = new Customer(3, "John Doe", invalidAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForDiscountLoyaltyCard() shouldBe Left(InvalidAge("Customer is too young"))
      }
    }
    "return Left" when {
      "the customer already have a  LoyaltyCard" in {
        val customer = new Customer(4, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.applyForDiscountLoyaltyCard() shouldBe Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return Left" when {
      "the minimum purchases is less than 5" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesUnder5)
        customer.applyForDiscountLoyaltyCard() shouldBe Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Left" when {
      "the minimum spent is less than 150" in {
        val customer = new Customer(3, "John Doe", validAge, invalidTotalSpend, totalPurchasesUnder5)
        customer.applyForDiscountLoyaltyCard() shouldBe Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Right" when {
      "the customer is of valid age, has no loyalty card and has made minimum purchases of 5, has " +
        "spent more than or equals to 150 " in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.applyForDrinksLoyaltyCard() shouldBe Right(DrinksLoyaltyCard(None))
      }
    }
  }

  "removeCurrentLoyaltyCard" should {
    "return Right" when {
      "the current DiscountLoyalty Card is removed" in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.removeCurrentLoyaltyCard() shouldBe Right(true)

      }
      "return Right" when {
        "the current DrinksLoyalty card is not removed" in {
          val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
          customer.setLoyaltyCard(Some(drinksLoyaltyCard))
          customer.removeCurrentLoyaltyCard() shouldBe Right(true)
        }
      }

    }
  }
  "applyForLoyaltyCard" should {
    "return Right"  when{
      "selected LoyaltyCard is DrinksLoyaltyCard"  in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForLoyaltyCard(LoyaltyCardType.DrinksLoyalty) shouldBe(Right(DrinksLoyaltyCard(None)))
      }
    }
    "return Right"  when{
      "selected LoyaltyCard is DiscountLoyaltyCard"  in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForLoyaltyCard(LoyaltyCardType.DiscountLoyalty) shouldBe(Right(DiscountLoyaltyCard(None)))
      }
    }
    "return Left"  when{
      "customer already has a DiscountLoyalty card"  in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.applyForLoyaltyCard(LoyaltyCardType.DiscountLoyalty) shouldBe(Left(AlreadyHasCard("You already have a loyalty card")))
      }
    }
    "return Left"  when{
      "customer already has a DrinksLoyalty card"  in {
        val customer = new Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(drinksLoyaltyCard))
        customer.applyForLoyaltyCard(LoyaltyCardType.DiscountLoyalty) shouldBe(Left(AlreadyHasCard("You already have a loyalty card")))
      }
    }


  }
}
