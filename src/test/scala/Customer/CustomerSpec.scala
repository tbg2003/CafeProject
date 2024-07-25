package Customer


import LoyaltyCard.DiscountLoyaltyCard
import LoyaltyCard.LoyaltyCardType.DrinksLoyalty
import Utils.POSError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CustomerSpec extends AnyWordSpec with Matchers {

  implicit val invalidAge:Int = 17
  implicit val validAge:Int = 19
  implicit val invalidTotalSpend:Double = 100.00
  implicit val validTotalSpend:Double = 160.00
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

  // not implemented
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
        customer.hasMadeMinPurchases() shouldBe Left(POSError.InvalidMinPurchases(""))
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

/** TO BE TESTED

 applyForLoyaltyCard
 setLoyaltyCard
 applyForLoyaltyCard
 applyForDrinksLoyaltyCard
 applyForDiscountLoyaltyCard
 hasLoyaltyCard
 hasSpentMinTotal
 removeCurrentLoyaltyCard

  */






}
