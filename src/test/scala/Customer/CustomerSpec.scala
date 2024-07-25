package Customer


import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CustomerSpec extends AnyWordSpec with Matchers {



  "getTotalSpent" should {
    "return the current total spent" in {
      assert(Customer.getTotalSpent() == mockTotalSpent)
    }
  }
}
