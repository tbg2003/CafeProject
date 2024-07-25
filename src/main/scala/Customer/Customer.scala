package Customer

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard, LoyaltyCard, LoyaltyCardType}
import Utils.POSError
import Utils.POSError.{InvalidAge, InvalidMinPurchases, InvalidMinSpendTotal}

case class Customer(id: Int,
                    fullName: String,
                    age: Int,
                    totalSpent: Double = 0,
                    totalPurchases: Int = 0,
                    loyaltyCard: Option[LoyaltyCard] = None) {

  private var currentTotalSpent: Double = totalSpent
  private var currentTotalPurchases: Int = totalPurchases
  private var currentLoyaltyCard: Option[LoyaltyCard] = loyaltyCard


  def getLoyaltyCard(): Option[LoyaltyCard] = {
    currentLoyaltyCard
  }

  def setLoyaltyCard(newCard: Option[LoyaltyCard]): Option[LoyaltyCard] = {
    currentLoyaltyCard = newCard match {
      case Some(card) => Some(card)
      case None => None
    }
    currentLoyaltyCard
  }

  def getTotalSpent(): Double = {
    currentTotalSpent
  }

  def getTotalPurchases(): Int = {
    currentTotalPurchases
  }

  def newOrder(purchaseAmount: Double): (Double, Int) = {
    currentTotalSpent += purchaseAmount
    currentTotalPurchases += 1
    Tuple2(currentTotalSpent, currentTotalPurchases)

  }

  def applyForLoyaltyCard(loyaltyCard: LoyaltyCardType): Either[POSError, LoyaltyCard] = {
    val maybeCard = loyaltyCard match {
      case LoyaltyCardType.DrinksLoyalty => applyForDrinksLoyaltyCard()
      case LoyaltyCardType.DiscountLoyalty => applyForDiscountLoyaltyCard()
    }
    maybeCard match {
      case Left(error) => Left(error)
      case Right(newCard) =>
        setLoyaltyCard(Some(newCard))
        Right(newCard)
    }
  }

  def applyForDrinksLoyaltyCard(): Either[POSError, LoyaltyCard] = {
    for {
      _ <- isValidAge()
      _ <- hasNoLoyaltyCard()
      _ <- hasMadeMinPurchases()
      newCard = DrinksLoyaltyCard(None)
    } yield newCard
  }

  def applyForDiscountLoyaltyCard(): Either[POSError, LoyaltyCard] = {
    for {
      _ <- isValidAge()
      _ <- hasNoLoyaltyCard()
      _ <- hasMadeMinPurchases()
      _ <- hasSpentMinTotal()
      newCard = DiscountLoyaltyCard(None)
    } yield newCard
  }

  def isValidAge(): Either[POSError, Boolean] = {
    if (age < 18) Left(InvalidAge("Customer is too young"))
    else {
      Right(true)
    }
  }

  def hasNoLoyaltyCard(): Either[POSError, Boolean] = {
    currentLoyaltyCard match {
      case Some(card) => Left(POSError.AlreadyHasCard("You already have a loyalty card"))
      case None => Right(true)
    }
  }

  def hasMadeMinPurchases(): Either[POSError, Boolean] = {
    if (currentTotalPurchases < 5) Left(InvalidMinPurchases("Minimum purchase less than 5 times"))
    else {
      Right(true)
    }
  }

  def hasSpentMinTotal(): Either[POSError, Boolean] = {
    if(currentTotalSpent<150){
      Left(InvalidMinSpendTotal("Minimum spent less than 150"))
    }
    else Right(true)
  }

    def removeCurrentLoyaltyCard(): Either[POSError, Boolean] = {
      ???
    }


  }
