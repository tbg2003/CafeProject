package MenuStuff

trait ItemType

object ItemType{
  case object Special extends ItemType
  case object HotFood extends ItemType
  case object ColdFood extends ItemType
  case object HotDrink extends ItemType
  case object ColdDrink extends ItemType
}
