package Staff

sealed class StaffMember(name:String, role:StaffRole)

object StaffMember{
  case object Waitress1 extends StaffMember("Waitress1", StaffRole.Waitress)
  case object Waiter1 extends StaffMember("Waiter1", StaffRole.Waiter)
  case object Manager1 extends StaffMember("Manager1", StaffRole.Manager)
}