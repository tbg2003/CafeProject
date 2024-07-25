package Bill

import LoyaltyCard.LoyaltyCard
import MenuStuff.{ItemType, MenuItem}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


case class Bill(
                order:List[MenuItem],
                payService:Boolean,
                loyaltyCard:Option[LoyaltyCard],
                extraTip:Option[Double]){

  def getOrderItemTypes():ListBuffer[ItemType] = {
    val itemTypeList:ListBuffer[ItemType] = ListBuffer()
    order.foreach(itemTypeList+=_.itemType)
    itemTypeList
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

  def applyServiceCharge():Double = {
    // if drinks loyaly then bill = apply drinks
    // if discount loyal = apply discount loyalty
    // else = sum bill
    // get service charge and *
    // if extra tip then add
    // return total
    ???
  }

  def applyDrinksLoyalty():Double = {
    // sum up items
    // if cold drink or hot drink in item types
    //    if get free drink is Right
    //        remove a drink price from bill
    //    else print Left error
    // return new bill price
    ???
  }

  def applyDiscountLoyalty():Double = {
    // bill = sum up items - sum up bill specials
    // apply discount to bill
    // + sum of bill specials
    // add star?
    // return new bill price
    ???
  }

  def getServiceCharge():Double={
    // if pay service then
      // get item type list
      // if contains Specials = 1.25
      // else if Hot Food = 1.2
      // else if Hot Drinks 1.1
      // else 1
    // else 1
    ???
  }
}
