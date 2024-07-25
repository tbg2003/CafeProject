package Bill

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}
import MenuStuff.{ItemType, MenuItem}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class BillSpec extends AnyWordSpec with Matchers{
  val ColdFood :MenuItem = MenuItem("Cold Food",1.00, ItemType.ColdFood)
  val HotFood :MenuItem = MenuItem("Hot Food", 2.00, ItemType.HotFood)
  val Special :MenuItem = MenuItem("Special", 3.00, ItemType.Special)
  val ColdDrink :MenuItem = MenuItem("Cold Drink", 4.00, ItemType.ColdDrink)
  val ColdDrink2 :MenuItem = MenuItem("Cold Drink", 4.00, ItemType.ColdDrink)
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
    "return 1.0 if pay service is false" in {
      val orderWithAllTypes:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, HotDrink)
      val bill:Bill = Bill(orderWithAllTypes, payService = false, None, None)
      bill.getServiceCharge() shouldBe 1.0
    }
    "return 1.25 if order contains at least one special item and service true" in {
      val orderWithAllTypes:List[MenuItem] = List(ColdFood, HotFood, Special)
      val bill:Bill = Bill(orderWithAllTypes, payService = true, None, None)
      bill.getServiceCharge() shouldBe 1.25
    }
    "return 1.2 if order contains at least one hot food item and no special item and service true" in {
      val orderHotFoodNoSpecial:List[MenuItem] = List(ColdFood, HotFood, ColdDrink, HotDrink)
      val bill:Bill = Bill(orderHotFoodNoSpecial, payService = true, None, None)
      bill.getServiceCharge() shouldBe 1.2
    }
    "return 1.1" when {
      "an order contains at least one hot drink and no hot food or special item and service true" in {
        val orderHotDrinkNoSpecialOrHotFood: List[MenuItem] = List(HotDrink, ColdDrink)
        val bill: Bill = Bill(orderHotDrinkNoSpecialOrHotFood, payService = true, None, None)
        bill.getServiceCharge() shouldBe 1.1
      }
      "an order contains at least one cold food and no hot food or special item and service true" in {
        val orderHotDrinkNoSpecialOrHotFood: List[MenuItem] = List(ColdFood, ColdDrink)
        val bill: Bill = Bill(orderHotDrinkNoSpecialOrHotFood, payService = true, None, None)
        bill.getServiceCharge() shouldBe 1.1
      }
    }
    "return 1.0 if order contains just cold drinks, i.e. no hot drink, cold food, hot food or special item and service true" in {
      val orderColdFoodNoSpecialOrHotFood:List[MenuItem] = List(ColdDrink)
      val bill:Bill = Bill(orderColdFoodNoSpecialOrHotFood, payService = true, None, None)
      bill.getServiceCharge() shouldBe 1.0
    }
  }

  "removeCheapestDrinkCost" should {
    "reduce bill by cost of cheapest drink item" when {
      "customer gets 10th stamp on drinks loyalty card" in {
        val coldDrink :MenuItem = MenuItem("Cold Drink", 1.00, ItemType.ColdDrink)
        val hotDrink :MenuItem = MenuItem("Cold Drink",2.00, ItemType.ColdDrink)
        val drinkOrder:List[MenuItem] = List(coldDrink, hotDrink)
        val bill:Bill = Bill(drinkOrder, payService = true, loyaltyCard = None, extraTip = None)
        val billTotalBeforeDiscount:Double = bill.sumUpBill()
        bill.removeCheapestDrinkCost(billTotalBeforeDiscount) shouldBe 2.00
      }
    }
    "not reduce the bill" when{
      "customer ordered no drinks" in {
        val hotFood:MenuItem = MenuItem("Hot Food", 10.00, ItemType.HotFood)
        val coldFood:MenuItem = MenuItem("Hot Food", 5.00, ItemType.ColdFood)
        val foodOrder:List[MenuItem] = List(hotFood, coldFood)
        val bill:Bill = Bill(foodOrder, payService = true, loyaltyCard = None, extraTip = None)
        val billTotalBeforeDiscount:Double = bill.sumUpBill()
        bill.removeCheapestDrinkCost(billTotalBeforeDiscount) shouldBe 15
      }
    }
  }

  "getFreeDrink" should {
    val date1: LocalDate = LocalDate.of(2024, 5, 1)
    val date2: LocalDate = LocalDate.of(2024, 5, 2)
    val date3: LocalDate = LocalDate.of(2024, 5, 3)
    val date4: LocalDate = LocalDate.of(2024, 5, 4)
    val date5: LocalDate = LocalDate.of(2024, 5, 5)
    val date6: LocalDate = LocalDate.of(2024, 5, 6)
    val date7: LocalDate = LocalDate.of(2024, 5, 7)
    val date8: LocalDate = LocalDate.of(2024, 5, 8)
    val date9: LocalDate = LocalDate.of(2024, 5, 9)
    val cardWith9Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9)
    val cardWith5Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5)
    val loyaltyCardWith9Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith9Stamps))
    val loyaltyCardWith5Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith5Stamps))
    "return true" when{
      "customer has drinks discount card and gets 10th stamp" in {}
    }
  }


  "applyDrinksLoyalty"


  "applyDiscountLoyalty"


  "getBillTotal"
}
