package Customer



import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}

import Utils.POSError.{AlreadyHasCard, InvalidAge, InvalidMinPurchases, InvalidMinSpendTotal}

import LoyaltyCard.DiscountLoyaltyCard
import LoyaltyCard.LoyaltyCardType.DrinksLoyalty

import Utils.POSError.{AlreadyHasCard, InvalidMinSpendTotal}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class CustomerSpec extends AnyWordSpec with Matchers {

  implicit val invalidAge:Int = 17
  implicit val validAge:Int = 19
  implicit val invalidTotalSpend:Double = 100.00
  implicit val validTotalSpend:Double = 160.00
  implicit val validTotalSpend150:Double = 150.00
  implicit val totalPurchasesUnder5:Int = 2
  implicit val totalPurchasesOver5:Int = 8
  implicit val emptyStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List()))


  val date1: LocalDate = LocalDate.of(2024, 5, 1)
  val date2: LocalDate = LocalDate.of(2024, 3, 2)
  val date3: LocalDate = LocalDate.of(2024, 5, 3)
  val date4: LocalDate = LocalDate.of(2024, 6, 4)
  val date5: LocalDate = LocalDate.of(2024, 5, 5)
  val fourStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2, date3, date4)))

//  val customer:Customer = Customer()

  "getTotalSpent" should {
    "return the current total spent" in {
      val customer:Customer = Customer(1, "John Doe", 18, totalSpent = 200)
      assert(customer.getTotalSpent() == 200)
    }
  }

  "getTotalPurchases" should {
    "return the current total purchases" in {
      val customer:Customer = Customer(1, "John Doe", 18, totalPurchases = 10)
      assert(customer.getTotalPurchases() == 10)
    }
  }

  "getLoyaltyCard" should {
    "return None" when {
      " there is no loyalty card assigned" in {
        val customer:Customer = Customer(1, "John Doe", 18)
        customer.getLoyaltyCard() shouldBe None
      }
    }
    "return discount loyalty card" when {
      "discount loyalty card is assigned" in {
        val customer:Customer = Customer(1, "John Doe", 18, loyaltyCard = Some(emptyStampDiscountLoyaltyCard))
        customer.getLoyaltyCard() shouldBe Some(emptyStampDiscountLoyaltyCard)
      }
    }
  }

  "newOrder" should {
    "update total spent with purchase amount, add 1 to total purchases" in {
      val customer:Customer = Customer(1, "John Doe", 18)
      assert(customer.newOrder(10.00) == (10.00, 1))
    }
  }

  "hasLoyaltyCard" should {
    "return left" when {
      "when customer has loyalty card" in {
        val customer:Customer = Customer(1, "John Doe", 18, loyaltyCard = Some(emptyStampDiscountLoyaltyCard))
        customer.hasLoyaltyCard() shouldBe Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return right" when {
      "when customer has no loyalty card" in {
        val customer:Customer = Customer(1, "John Doe", 18)
        customer.hasLoyaltyCard() shouldBe Right(true)
      }
    }
  }

  "isValidAge" should {
    "return a Left" when{
      "customer's age doesn't meet valid age requirement" in {
        val customer:Customer = Customer(1, "John Doe", invalidAge)
        customer.isValidAge() shouldBe Left(POSError.InvalidAge("Customer is too young"))
      }
    }
    "return a Right" when{
      "customer's age meets valid age requirement" in {
        val customer: Customer = Customer(1, "John Doe", validAge)
        customer.isValidAge() shouldBe Right(true)
      }
    }
  }

  "hasMadeMinPurchases" should {
    "return a Left" when{
      "customer's total purchases is under 5" in {
        val customer:Customer = Customer(1, "John Doe", validAge, validTotalSpend, totalPurchasesUnder5)
        customer.hasMadeMinPurchases() shouldBe Left(POSError.InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return a Right" when{
      "customer's total purchases is over 5" in {
        val customer:Customer = Customer(1, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.hasMadeMinPurchases() shouldBe Right(true)
      }
      "customer's total purchases is 5" in {
        val customer:Customer = Customer(1, "John Doe", validAge, validTotalSpend, 5)
        customer.hasMadeMinPurchases() shouldBe Right(true)
      }
    }
  }
  "hasSpentMinTotal" should {
    "return a Left" when{
      "customers total purchases is less than 150" in {
        val customer = Customer(3, "John Doe", validAge, invalidTotalSpend, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Left(InvalidMinSpendTotal("Minimum spent less than 150"))
      }
    }
    "return a Right" when{
      "customers total purchases is greater than 150" in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Right(true)
      }
    }
    "return a Right" when{
      "customers total purchases is exactly 150" in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.hasSpentMinTotal() shouldBe Right(true)
      }
    }
  }
  "applyForDrinksLoyaltyCard" should {
    "return Left" when{
      "the age is less than 18" in {
        val customer = Customer(3, "John Doe", invalidAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForDrinksLoyaltyCard() shouldBe  Left(InvalidAge("Customer is too young"))
      }
    }
    "return Left" when{
      "the customer already has  LoyaltyCard" in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.applyForDrinksLoyaltyCard() shouldBe  Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return Left" when{
      "the minimum purchases is less than 5" in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesUnder5)
        customer.applyForDrinksLoyaltyCard() shouldBe  Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Right" when{
      "the customer is of valid age, has no loyalty card and has made minimum purchases " in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        //customer.setLoyaltyCard()
        customer.applyForDrinksLoyaltyCard() shouldBe  Right(DrinksLoyaltyCard(None))
      }
    }
  }


  "applyForDiscountLoyaltyCard" should {
    "return Left" when{
      "the age is less than 18" in {
        val customer = Customer(3, "John Doe", invalidAge, validTotalSpend150, totalPurchasesOver5)
        customer.applyForDiscountLoyaltyCard() shouldBe  Left(InvalidAge("Customer is too young"))
      }
    }
    "return Left" when{
      "the customer already have a  LoyaltyCard" in {
        val customer = Customer(4, "John Doe", validAge, validTotalSpend150, totalPurchasesOver5)
        customer.setLoyaltyCard(Some(fourStampDiscountLoyaltyCard))
        customer.applyForDiscountLoyaltyCard() shouldBe  Left(AlreadyHasCard("You already have a loyalty card"))
      }
    }
    "return Left" when{
      "the minimum purchases is less than 5" in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend150, totalPurchasesUnder5)
        customer.applyForDiscountLoyaltyCard() shouldBe  Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Left" when{
      "the minimum spent is less than 150" in {
        val customer = Customer(3, "John Doe", validAge, invalidTotalSpend, totalPurchasesUnder5)
        customer.applyForDiscountLoyaltyCard() shouldBe  Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
      }
    }
    "return Right" when{
      "the customer is of valid age, has no loyalty card and has made minimum purchases of 5, has " +
        "spent more than or equals to 150 " in {
        val customer = Customer(3, "John Doe", validAge, validTotalSpend, totalPurchasesOver5)
        customer.applyForDrinksLoyaltyCard() shouldBe  Right(DrinksLoyaltyCard(None))
      }
    }
  }


/** TO BE TESTED

 applyForLoyaltyCard
 setLoyaltyCard
 applyForLoyaltyCard
 applyForDiscountLoyaltyCard
 hasLoyaltyCard
 removeCurrentLoyaltyCard

  */






}
