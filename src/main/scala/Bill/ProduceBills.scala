package Bill

import Staff.StaffMember
import Utils.POSError
import Utils.POSError.InvalidBillAmount

import java.time.{LocalDateTime, LocalTime}

class ProduceBills(bill: Bill){

  def generateReceipt(staffMember: StaffMember, transactionType: TransactionType):Either[POSError, Receipt] = {
    val afterDiscount = bill.getBillTotal(LocalDateTime.now())
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


