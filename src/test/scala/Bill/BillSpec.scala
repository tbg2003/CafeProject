package Bill

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}
import MenuStuff.{ItemType, MenuItem}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class BillSpec extends AnyWordSpec with Matchers{

  val date1: LocalDate = LocalDate.of(2024, 5, 1)
  val date2: LocalDate = LocalDate.of(2024, 5, 2)
  val date3: LocalDate = LocalDate.of(2024, 5, 3)
  val date4: LocalDate = LocalDate.of(2024, 5, 4)
  val date5: LocalDate = LocalDate.of(2024, 5, 5)
  val date6: LocalDate = LocalDate.of(2024, 5, 6)
  val date7: LocalDate = LocalDate.of(2024, 5, 7)
  val date8: LocalDate = LocalDate.of(2024, 5, 8)
  val date9: LocalDate = LocalDate.of(2024, 5, 9)
  val today: LocalDate = LocalDate.now()
  val cardWith9Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5, date6, date7, date8, date9)
  val cardWith5Stamps:List[LocalDate] = List(date1, date2, date3, date4, date5)
  val cardWithTodayStamp:List[LocalDate] = List(date1, date2, date3, date4, today)
  val validLoyaltyCardWith9Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith9Stamps))
  val loyaltyCardWith5Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith5Stamps))
  val loyaltyCardWithTodayStamp:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWithTodayStamp))


  val ColdFood :MenuItem = MenuItem("Cold Food",1.00, ItemType.ColdFood)
  val HotFood :MenuItem = MenuItem("Hot Food", 2.00, ItemType.HotFood)
  val Special :MenuItem = MenuItem("Special", 3.00, ItemType.Special)
  val ColdDrink :MenuItem = MenuItem("Cold Drink", 4.00, ItemType.ColdDrink)
  val cheapestColdDrink :MenuItem = MenuItem("Cold Drink", 2.00, ItemType.ColdDrink)
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

    "return true" when{
      "customer has drinks discount card and gets 10th stamp" in {
        val orderWithDrinks:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, cheapestColdDrink, HotDrink)
        def bill:Bill = Bill(orderWithDrinks, payService = true, None, None)
        bill.getFreeDrink(validLoyaltyCardWith9Stamps) shouldBe true
      }
    }
    "return false" when{
      "customer has invalid number of stamps on drinks discount card" in {
        val orderWithDrinks:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, cheapestColdDrink, HotDrink)
        def bill:Bill = Bill(orderWithDrinks, payService = true, None, None)
        bill.getFreeDrink(loyaltyCardWith5Stamps) shouldBe false
      }
      "customer has already had stamp today on drinks discount card" in {
        val orderWithDrinks:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, cheapestColdDrink, HotDrink)
        def bill:Bill = Bill(orderWithDrinks, payService = true, None, None)
        bill.getFreeDrink(loyaltyCardWithTodayStamp) shouldBe false
      }
    }
  }

  "applyDrinksLoyalty" should {
    "remove cost of cheapest drink" when {
      "customer has ordered a cold or hot drink and has drinks discount loyalty card with 9 stamps" in {
        val loyaltyCardWith9Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith9Stamps))
        val orderWithDrinks:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, cheapestColdDrink, HotDrink)
        val bill:Bill = Bill(orderWithDrinks, payService = true, None, None)
        bill.applyDrinksLoyalty(loyaltyCardWith9Stamps) shouldBe (bill.sumUpBill()-cheapestColdDrink.price)
      }
    }
    "not cost of cheapest drink" when {
      "customer has a drinks discount loyalty card with 9 stamps but has not ordered a cold or hot drink" in {
        val loyaltyCardWith9Stamps:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith9Stamps))
        val orderWithNoDrinks:List[MenuItem] = List(ColdFood, HotFood, Special)
        val bill:Bill = Bill(orderWithNoDrinks, payService = true, None, None)
        bill.applyDrinksLoyalty(loyaltyCardWith9Stamps) shouldBe bill.sumUpBill()
      }
      "customer has not ordered a cold or hot drink but doesn't have a valid drinks discount loyalty card" in {
        val invalidLoyaltyCard:DrinksLoyaltyCard = DrinksLoyaltyCard(Some(cardWith5Stamps))
        val orderWithDrinks:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, cheapestColdDrink, HotDrink)
        val bill:Bill = Bill(orderWithDrinks, payService = true, None, None)
        bill.applyDrinksLoyalty(invalidLoyaltyCard) shouldBe bill.sumUpBill()
      }
    }
  }

  "applyDiscountLoyalty" should {
    val coldFoodPrice10 :MenuItem = MenuItem("Cold Food",10.00, ItemType.ColdFood)
    val hotFoodPrice10 :MenuItem = MenuItem("Hot Food", 10.00, ItemType.HotFood)
    val specialPrice20 :MenuItem = MenuItem("Special", 20.00, ItemType.Special)
    val coldDrinkPrice10 :MenuItem = MenuItem("Cold Drink", 10.00, ItemType.ColdDrink)
    val hotDrinkPrice10 :MenuItem = MenuItem("Hot Drink", 10.00, ItemType.HotDrink)


    val orderWithSpecial:List[MenuItem] = List(coldFoodPrice10, hotFoodPrice10, specialPrice20, coldDrinkPrice10, hotDrinkPrice10)
    val orderWithOutSpecial:List[MenuItem] = List(coldFoodPrice10, hotFoodPrice10, coldDrinkPrice10, hotDrinkPrice10)


    "return bill value after applying 2% discount per star on all non-special items" when{
      "customer has valid discount loyalty card with" in {
        val twoStampDiscountLoyaltyCard1: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2)))
        val twoStampDiscountLoyaltyCard2: DiscountLoyaltyCard = DiscountLoyaltyCard(Some(List(date1, date2)))
        val bill:Bill = Bill(orderWithSpecial, payService = true, None, None)
        val bill2:Bill = Bill(orderWithOutSpecial, payService = true, None, None)
        bill2.applyDiscountLoyalty(twoStampDiscountLoyaltyCard1) shouldBe 38.40
        //all items total 60, non special items total 40, 2 stars = 4% discount off non special, 1.6 discount, 60 - 1.6 = 58.40
        bill.applyDiscountLoyalty(twoStampDiscountLoyaltyCard2) shouldBe 58.40
      }
    }
  }




  "getBillTotal" should {
    "reduce bill by cost of cheapest drink" when {
      "customer has valid drinks loyalty card and ordered drinks" in {}
    }
    "reduce bill by percentage off non special items" when {
      "customer has valid discount card" in {}
    }
    "reduce second bill (non special items) by 2% more after spending £20 on first bill" when {
      "customer has valid discount card with less that 8 stars" in {}
    }
    "reduce third bill (non special items) by 2% more after spending £20 on first and second bill" when {
      "customer has valid discount card and maximum 6 stars" in {}
    }
    "reduce third bill (non special items) by same percentage as second bill after first bill" when {
      "customer has valid discount card and only first bill over 20 " in {}
      "customer has valid discount card and first, third bill over 20" in{}
      "customer has 8 stars on discount card"
    }
    "reduce second bill (non special items) by same amount as first " when {
      "customer has valid discount card and first bill under 20" in {}
      "customer has valid discount card with 8 stars" in {}
    }
    "don't reduce bill" when {
      "user has 9 stamps on drinks loyalty card but no drinks ordered" in {}
      "user has ordered drinks but no 9 stamps on drinks loyalty card" in {}
      "user has valid discount loyalty card but no stars" in {}
      "user has valid discount loyalty with stars but only ordered special items" in {}
    }
  }
}
