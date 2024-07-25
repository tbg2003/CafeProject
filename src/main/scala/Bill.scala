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


  def applyServiceChargeWithDrinksLoyalty():Double = {
    // apply discounts from loyalty card
    ???
  }

  def applyDiscounts():Double = {
    // if drinks loyalty card
    // if discount loyalty card
    ???
  }
}
