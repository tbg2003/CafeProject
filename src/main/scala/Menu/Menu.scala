package Menu

case class Menu(
                 menuItems: List[MenuItem]
               ){

  private var menuList:List[MenuItem] = menuItems

  def getMenu:List[MenuItem] = {
    ???
  }
  def addSpecialItem(itemToAdd:MenuItem):List[MenuItem] = {
    ???
  }
  def removeSpecialItem(itemToRemove:MenuItem):List[MenuItem] = {
    ???
  }
}
