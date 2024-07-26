package Bill

import Customer.Customer
import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard}
import MenuStuff.{ItemType, MenuItem}
import Staff.StaffMember
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{LocalDate, LocalDateTime, LocalTime}

class ProduceBillsSpec extends AnyWordSpec with Matchers{

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

  val notHappyHourTime: LocalDateTime = LocalDateTime.of(date1, LocalTime.of(12,12,12))
  val happyHourTime: LocalDateTime = LocalDateTime.of(LocalDate.now(), LocalTime.of(18,0,0))

  // totals price 16
  val ColdFood10 :MenuItem = MenuItem("Cold Food",10.00, ItemType.ColdFood)
  val HotFood10 :MenuItem = MenuItem("Hot Food", 10.00, ItemType.HotFood)
  val Special20 :MenuItem = MenuItem("Special", 20.00, ItemType.Special)
  val ColdDrink10 :MenuItem = MenuItem("Cold Drink", 10.00, ItemType.ColdDrink)
  val cheapestColdDrink5 :MenuItem = MenuItem("Cold Drink", 5.00, ItemType.ColdDrink)
  val HotDrink10 :MenuItem = MenuItem("Hot Drink", 10.00, ItemType.HotDrink)
  // total 65
  // drinks 25 food 20 special 20
  val mixedOrderWithSpecial:List[MenuItem] = List(ColdFood10, HotFood10, Special20, ColdDrink10, cheapestColdDrink5, HotDrink10)
  // total 45
  val mixedOrderWithOutSpecial:List[MenuItem] = List(ColdFood10, HotFood10, ColdDrink10, cheapestColdDrink5, HotDrink10)

  val orderTotal15:List[MenuItem] = List(ColdFood10, cheapestColdDrink5)
   val customer: Customer = new Customer(1, "John Doe", 18, totalSpent = 200.0, totalPurchases = 20)


  "generateReceipt" should{
    "return Right" when{
      "the bill is valid and has total greater than 0" in {
        val nineStampedDrinksLoyaltyCard: DrinksLoyaltyCard = DrinksLoyaltyCard(Some(List(date1, date2, date3, date4, date5, date6, date7,date8, date9)))
        val billDrinksLoyalty: Bill =Bill(customer, mixedOrderWithSpecial, payService = false, loyaltyCard = Some(nineStampedDrinksLoyaltyCard), extraTip = None)
        val produceBills = new ProduceBills(billDrinksLoyalty)
        val amountAfterDiscount = billDrinksLoyalty.getBillTotal(notHappyHourTime)
        val testReceipt = Receipt.apply(timeOfTransaction = LocalTime.now,
          staffMember = StaffMember.Waiter1, transactionType = TransactionType.Card, 60,65)
        produceBills.generateReceipt(staffMember = StaffMember.Waiter1, TransactionType.Card, notHappyHourTime) shouldBe(testReceipt)
      }
    }
  }


}
