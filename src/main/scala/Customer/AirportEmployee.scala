package Customer


import LoyaltyCard.LoyaltyCard
import Utils.POSError
import Utils.POSError.InvalidServicePeriod

import java.time.{LocalDate, Period}


class AirportEmployee(
                       val id: Int, val fullName: String, val age: Int,
                       val totalSpent: Double = 0, val totalPurchases: Int = 0,
                       val loyaltyCard: Option[LoyaltyCard], startDate: LocalDate) extends Customer(
  id, fullName, age, totalSpent, totalPurchases, loyaltyCard) {


  def validateEmployeeTenure(): Either[POSError, Int] = {
    val period = Period.between(startDate, LocalDate.now)
    val totalMonths= period.getYears*12 + period.getMonths
    if(totalMonths >= 6){
      Right(10)
    }
    else {
      Left(InvalidServicePeriod("Not eligible for airport discount"))
    }
  }

}

