package MenuStuff
import Utils.POSError

case class Menu(
                 menuItems: List[MenuItem]
               ){

  private var menuList:List[MenuItem] = menuItems

  def getMenu:List[MenuItem] = {
    menuList
  }
  def addSpecialItem(itemToAdd:MenuItem):Either[POSError, List[MenuItem]] = {
    if (itemToAdd.itemType == ItemType.Special) Right(menuList:+ itemToAdd)
    else Left(POSError.InvalidItemType(s"${itemToAdd.name} is not a special item"))
  }
  def removeSpecialItem(itemToRemove:MenuItem):Either[POSError, List[MenuItem]] = {
    if (itemToRemove.itemType == ItemType.Special) Right(menuList.filterNot(_ == itemToRemove))
    else Left(POSError.InvalidItemType(s"${itemToRemove.name} is not a special item"))
  }
}