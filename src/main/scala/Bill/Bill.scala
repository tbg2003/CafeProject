package Bill

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard, LoyaltyCard}
import MenuStuff.ItemType.{ColdDrink, HotDrink}
import MenuStuff.{ItemType, MenuItem}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


case class Bill(
                order:List[MenuItem],
                payService:Boolean,
                loyaltyCard:Option[LoyaltyCard],
                extraTip:Option[Double]){

  def getOrderItemTypes():List[ItemType] = {
    val itemTypeList:ListBuffer[ItemType] = ListBuffer()
    order.foreach(itemTypeList+=_.itemType)
    itemTypeList.toList
  }

  def sumUpBillSpecials():Double = {
    @tailrec
    def helpSumBillSpecials(order:List[MenuItem], acc:Double = 0):Double = {
      order match {
        case Nil => acc
        case ::(head, tail) if head.itemType == ItemType.Special => helpSumBillSpecials(tail, acc+head.price)
        case ::(head, tail) => helpSumBillSpecials(tail, acc)
      }
    }
    helpSumBillSpecials(order)
  }

  def sumUpBill():Double = {
    @tailrec
    def helpSumBill(order:List[MenuItem], acc:Double = 0):Double = {
      order match {
        case Nil => acc
        case ::(head, tail) => helpSumBill(tail, acc+head.price)
      }
    }
    helpSumBill(order)
  }

  def getBillTotal():Double = {
    val billDiscounted:Double = loyaltyCard match {
      case Some(card:DrinksLoyaltyCard) => applyDrinksLoyalty(card)
      case Some(card:DiscountLoyaltyCard) => applyDiscountLoyalty(card)
      case None => sumUpBill()
    }
    val billWithService:Double = billDiscounted * getServiceCharge()
    val billTotal:Double = extraTip match {
      case Some(tip) => billWithService + tip
      case None => billWithService
    }
    billTotal
  }

  def getFreeDrink(card: DrinksLoyaltyCard):Boolean = {
    card.getFreeDrink() match {
      case Left(error) => false
      case Right(value) => true
    }
  }

  def removeCheapestDrinkCost(billToDiscount:Double):Double = {
    val listNotDrinks:List[MenuItem] = order.filterNot(_.itemType == ColdDrink).filterNot(_.itemType == HotDrink)
    val listDrinks:List[MenuItem] = order.diff(listNotDrinks)
    val discountedBill:Double = if (listDrinks.nonEmpty){
      val cheapestDrinkCost:Double = listDrinks.minBy(_.price).price
      billToDiscount - cheapestDrinkCost
    }
    else billToDiscount
    discountedBill
  }

  def applyDrinksLoyalty(card: DrinksLoyaltyCard):Double = {
    val billToDiscount:Double = sumUpBill()
    val itemTypes:List[ItemType] = getOrderItemTypes()

    val billWithDiscount:Double =
      if (itemTypes.contains(ItemType.ColdDrink)||itemTypes.contains(ItemType.HotDrink)){
        if (getFreeDrink(card)) {
          val billWithDrinkRemoved:Double = removeCheapestDrinkCost(billToDiscount)
          billWithDrinkRemoved
        }
        else billToDiscount
      }
      else billToDiscount

    billWithDiscount
  }

  def applyDiscountLoyalty(card: DiscountLoyaltyCard):Double = {
    // bill = sum up items - sum up bill specials
    val costOfSpecials:Double = sumUpBillSpecials()
    val costOfOrder:Double = sumUpBill()
    val billToDiscount:Double = costOfOrder - costOfSpecials
    // apply discount to bill
    val discountedBill = billToDiscount * (1+card.getDiscount())
    // + sum of bill specials
    val discountedBillWithSpecials = discountedBill + costOfSpecials
    // add star?
    card.addStar(costOfOrder)
    // return new bill price
    discountedBillWithSpecials
  }

  def getServiceCharge():Double={
    // if pay service then
    val orderItemTypes:List[ItemType] = getOrderItemTypes()
    val serviceCharge:Double =
      if (payService){
        if(orderItemTypes.contains(ItemType.Special))1.25
        else if(orderItemTypes.contains(ItemType.HotFood))1.2
        else if(orderItemTypes.contains(ItemType.HotDrink) || orderItemTypes.contains(ItemType.ColdFood))1.1
        else 1.0
    } else 1.0
    serviceCharge
  }
}