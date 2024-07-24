
import Menu.{ItemType, Menu, MenuItem}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MenuSpec extends AnyWordSpec with Matchers{
  case object ColdFood extends MenuItem("Cold Food", 8.00, ItemType.ColdFood)
  case object HotFood extends MenuItem("Hot Food", 12.00, ItemType.HotFood)
  case object Special extends MenuItem("Special", 25.00, ItemType.Special)
  case object ColdDrink extends MenuItem("Cold Drink", 3.00, ItemType.ColdDrink)
  case object HotDrink extends MenuItem("Hot Drink", 2.00, ItemType.HotDrink)

  val menuItems:List[MenuItem] = List(ColdFood, HotFood, Special, ColdDrink, HotDrink)

  case object Menu extends Menu(menuItems)

  "getMenu" should {
    "return list of item names and price on menu" in {
      assert(Menu.getMenu == List(ColdFood, HotFood, Special, ColdDrink, HotDrink))
    }
  }

  "addSpecialItem" should {
    case object NewNotSpecialItem extends MenuItem("New Not Special", 25.00, ItemType.HotFood)
    case object NewSpecialItem extends MenuItem("New Special", 25.00, ItemType.Special)
    "return a Left" when{
      "Item is not special" in {
        assert(Menu.addSpecialItem(NewNotSpecialItem) == Left(POSError.InvalidItemType))
      }

    }
    "return a Right" when{
      "Item is special" in {
        assert(Menu.addSpecialItem(NewSpecialItem) == Right(???))
      }
    }
  }

  "removeSpecialItem"  should {
    case object NewNotSpecialItem extends MenuItem("New Not Special", 25.00, ItemType.HotFood)
    case object NewSpecialItem extends MenuItem("New Special", 25.00, ItemType.Special)
    "return a Left" when{
      "Item is not special" in {
        assert(Menu.removeSpecialItem(NewNotSpecialItem) == Left(POSError.InvalidItemType))
      }
    }
    "return a Right" when{
      "Item is special" in {
        assert(Menu.removeSpecialItem(NewSpecialItem) == Right(???))
      }
    }
  }
}
