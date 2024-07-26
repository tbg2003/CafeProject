package Bill

import Staff.StaffMember
import Error.POSError
import Error.POSError.InvalidBillAmount

import java.time.{LocalDateTime, LocalTime}

class ProduceBills(bill: Bill){

  def generateReceipt(staffMember: StaffMember, transactionType: TransactionType, happyHourOrNot:LocalDateTime):Either[POSError, Receipt] = {
    var afterDiscount = bill.getBillTotal(happyHourOrNot)
    val beforeDiscount = bill.sumUpBill()
    if(afterDiscount<0) {
      Left(InvalidBillAmount("Bill amount cannot be negative"))
    }
    else{
      val receipt = Receipt.apply(timeOfTransaction = LocalTime.now,
        staffMember = staffMember, transactionType = transactionType, afterDiscount, beforeDiscount)
      Right(receipt)
    }
  }
}


