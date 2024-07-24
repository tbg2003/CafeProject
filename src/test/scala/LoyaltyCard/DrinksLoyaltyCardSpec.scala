package LoyaltyCard

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DrinksLoyaltyCardSpec extends AnyWordSpec with Matchers{
  "addStamp" should{
    "Return a Right" when{
      "Adding stamp on a new day" in{
        val loyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard()
        DrinksLoyaltyCard.
      }
    }
  }
  "checkEnoughStamps"
  "getFreeDrink"
}
