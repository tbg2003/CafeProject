package MenuStuff
import Error.POSError

case class Menu(
                 menuItems: List[MenuItem]
               ){

  private var menuList:List[MenuItem] = menuItems

  def getMenu:List[MenuItem] = {
    menuList
  }
  def addSpecialItem(itemToAdd:MenuItem):Either[POSError, List[MenuItem]] = {
    if (itemToAdd.itemType == ItemType.Special){
      menuList = menuList:+ itemToAdd
      Right(menuList)
    } else Left(POSError.InvalidItemType(s"${itemToAdd.name} is not a special item"))
  }
  def removeSpecialItem(itemToRemove:MenuItem):Either[POSError, List[MenuItem]] = {
    if (itemToRemove.itemType == ItemType.Special){
      menuList = menuList.filterNot(_ == itemToRemove)
      Right(menuList)
    }
    else Left(POSError.InvalidItemType(s"${itemToRemove.name} is not a special item"))
  }
}