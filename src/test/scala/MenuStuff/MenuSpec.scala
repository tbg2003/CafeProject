package MenuStuff




import Utils.POSError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MenuSpec extends AnyWordSpec with Matchers{
  val ColdFood :MenuItem = MenuItem("Cold Food", 8.00, ItemType.ColdFood)
  val HotFood :MenuItem = MenuItem("Hot Food", 12.00, ItemType.HotFood)
  val Special :MenuItem = MenuItem("Special", 25.00, ItemType.Special)
  val ColdDrink :MenuItem = MenuItem("Cold Drink", 3.00, ItemType.ColdDrink)
  val HotDrink :MenuItem = MenuItem("Hot Drink", 2.00, ItemType.HotDrink)

  val menuItems:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, HotDrink)

  val menu:Menu = Menu(menuItems)

  "getMenu" should {
    "return list of item names and price on menu" in {
      assert(menu.getMenu == List(ColdFood, HotFood, Special, ColdDrink, HotDrink))
    }
  }

  "addSpecialItem" should {
    val newNotSpecialItem: MenuItem = MenuItem("New Not Special", 25.00, ItemType.HotFood)
    val newSpecialItem:MenuItem = MenuItem("New Special", 25.00, ItemType.Special)
    "return a Left" when{
      "Item is not special" in {
        menu.addSpecialItem(newNotSpecialItem) shouldBe Left(POSError.InvalidItemType("New Not Special is not a special item"))
      }

    }
    "return a Right" when{
      "Item is special" in {
        menu.addSpecialItem(newSpecialItem) shouldBe Right(List(ColdFood, HotFood, Special, ColdDrink, HotDrink, newSpecialItem))
      }
    }
  }

  "removeSpecialItem"  should {
    val newNotSpecialItem: MenuItem = MenuItem("New Not Special", 25.00, ItemType.HotFood)
    "return a Left" when{
      "Item is not special" in {
        menu.removeSpecialItem(newNotSpecialItem) shouldBe Left(POSError.InvalidItemType("New Not Special is not a special item"))
      }
    }
    "return a Right" when{
      "Item is special" in {
        menu.removeSpecialItem(Special) shouldBe Right(List(ColdFood, HotFood, ColdDrink, HotDrink))
      }
    }
  }
}
