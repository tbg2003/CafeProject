package Bill

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}
import MenuStuff.{ItemType, MenuItem}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BillSpec extends AnyWordSpec with Matchers{
  val ColdFood :MenuItem = MenuItem("Cold Food",1.00, ItemType.ColdFood)
  val HotFood :MenuItem = MenuItem("Hot Food", 2.00, ItemType.HotFood)
  val Special :MenuItem = MenuItem("Special", 3.00, ItemType.Special)
  val ColdDrink :MenuItem = MenuItem("Cold Drink", 4.00, ItemType.ColdDrink)
  val HotDrink :MenuItem = MenuItem("Hot Drink", 4.00, ItemType.HotDrink)

  val order:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, HotDrink)
  val emptyStampDiscountLoyaltyCard: DiscountLoyaltyCard = DiscountLoyaltyCard(None)
  val emptyStampDrinksLoyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(None)

  "getOrderItemTypes" should {
    "returns a list of item types of the order" in {
      val bill:Bill = Bill(order, payService = true, None, None)
      bill.getOrderItemTypes() shouldBe List(ItemType.ColdFood, ItemType.HotFood, ItemType.Special, ItemType.ColdDrink, ItemType.HotDrink)
    }
  }
  "sumUpBillSpecials" should {
    "return the total cost of all specials in order" in {
      val bill:Bill = Bill(order, payService = true, None, None)
      bill.sumUpBillSpecials() shouldBe 3.0
    }
  }
  "sumUpBill" should {
    "return the total cost of all items in order" in {
      val bill: Bill = Bill(order, payService = true, None, None)
      bill.sumUpBill() shouldBe 14
    }
  }
  "getServiceCharge" should{
    "return 1.0 if pay service is false" in {}
    "return 1.25 if order contains at least one special item" in {}
    "return 1.2 if order contains at least one hot food item and no special item" in {}
    "return 1.1 if order contains at least one hot drink item and no hot food or special item" in {}
    "return 1.0 if order contains no hot drink, hot food or special item" in {}
  }
  "removeCheapestDrinkCost"
  "getFreeDrink"
  "applyDrinksLoyalty"
  "applyDiscountLoyalty"
  "getBillTotal"
}
