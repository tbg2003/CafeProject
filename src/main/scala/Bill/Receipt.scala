package Bill

import Staff.StaffMember

import java.time.LocalTime

case class Receipt(timeOfTransaction:LocalTime,
                   staffMember:StaffMember, transactionType: TransactionType,
                   afterDiscount:Double, beforeDiscount:Double)
