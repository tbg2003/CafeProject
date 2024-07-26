package Staff

sealed trait StaffRole

object StaffRole{
  case object Waiter extends StaffRole
  case object Waitress extends StaffRole
  case object Manager extends StaffRole}
