package Customer


import LoyaltyCard.DiscountLoyaltyCard
import LoyaltyCard.LoyaltyCardType.DrinksLoyalty
import Utils.POSError
import Utils.POSError.{AlreadyHasCard, InvalidMinSpendTotal}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CustomerSpec extends AnyWordSpec with Matchers {

  implicit val invalidAge:Int = 17
  implicit val validAge:Int = 19
  implicit val invalidTotalSpend:Double = 100.00
  implicit val validTotalSpend:Double = 160.00
  implicit val validTotalSpend150:Double = 150.00
  implicit val totalPurchasesUnder5:Int = 2
  implicit val totalPurchasesOver5:Int = 8
  implicit val emptyStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List()))

  // implemented
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

/** TO BE TESTED

 applyForLoyaltyCard
 setLoyaltyCard
 applyForLoyaltyCard
 applyForDiscountLoyaltyCard
 hasLoyaltyCard
 removeCurrentLoyaltyCard

  */






}
