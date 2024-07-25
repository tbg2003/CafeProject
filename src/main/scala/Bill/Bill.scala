package Bill

import LoyaltyCard.LoyaltyCard
import MenuStuff.{ItemType, MenuItem}

import scala.collection.mutable.ListBuffer


case class Bill(order:List[MenuItem],
                payService:Boolean,
                loyaltyCard:Option[LoyaltyCard],
                extraTip:Option[Double]){

  def getOrderItemTypes():ListBuffer[ItemType] = {
    val itemTypeList:ListBuffer[ItemType] = ListBuffer()
    order.foreach(itemTypeList+=_.itemType)
    itemTypeList
  }

  def sumUpBillSpecials():Double = {
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
    def helpSumBill(order:List[MenuItem], acc:Double = 0):Double = {
      order match {
        case Nil => acc
        case ::(head, tail) => helpSumBill(tail, acc+head.price)
      }
    }
    helpSumBill(order)
  }

  def applyServiceCharge():Double = {
    // apply discounts from loyalty card
    ???
  }

  def applyServiceWithDrinksLoyalty():Double = {
    // sum up items
    // if cold drink or hot drink in item types
    //    if get free drink
    //        remove cheapest drink price from menu
    // return new bill price
    ???
  }

  def applyServiceWithDiscountLoyalty():Double = {
    // sum up items - sum up bill specials
    // apply discount to bill
    // + sum of bill specials
    // add star?
    // return new bill price
    ???
  }

  def getServiceCharge():Int={}
}
