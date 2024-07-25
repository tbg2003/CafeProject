package Customer


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

//  val customer:Customer = Customer()

  "getTotalSpent" should {
    "return the current total spent" in {
      val customer:Customer = Customer(1, "John Doe", 18, totalSpent = 200)
      assert(customer.getTotalSpent() == 200)
    }
  }
  "getTotalPurchases"
  "newOrder"
  "applyForLoyaltyCard"
  "applyForDrinksLoyaltyCard"
  "applyForDiscountLoyaltyCard"
  "isValidAge"
  "hasLoyaltyCard"
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
  }// should return left when invalid min purchases in // should return right when valid min purchases in
  "hasSpentMinTotal"
  "removeCurrentLoyaltyCard"

}
