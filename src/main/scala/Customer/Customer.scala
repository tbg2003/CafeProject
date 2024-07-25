package Customer

import LoyaltyCard.{DiscountLoyaltyCard, DrinksLoyaltyCard, LoyaltyCard, LoyaltyCardType}
import Utils.POSError

case class Customer(id: Int, fullName: String, age:Int, totalSpent: Double = 0, totalPurchases: Int = 0) {

  private var currentTotalSpent: Double = totalSpent
  private var currentTotalPurchases: Int = totalPurchases
  private var currentLoyaltyCard: Option[LoyaltyCard] = None


  def getLoyaltyCard(): Option[LoyaltyCard] = {
    currentLoyaltyCard
  }

  def setLoyaltyCard(newCard:Option[LoyaltyCard]): Option[LoyaltyCard] = {
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
    for{
      _ <-isValidAge()
      _ <-hasLoyaltyCard()
      _ <-hasMadeMinPurchases()
      newCard = DrinksLoyaltyCard(None)
    } yield newCard
  }

  def applyLoyaltyCard(loyaltyCard: LoyaltyCardType): Either[POSError, LoyaltyCard] = {
    for{
      _ <-isValidAge()
      _ <-hasLoyaltyCard()
      _ <-hasMadeMinPurchases()
      _ <- if(loyaltyCard == LoyaltyCardType.DiscountLoyalty) hasSpentMinTotal() else Right()
      newCard = if (loyaltyCard == LoyaltyCardType.DiscountLoyalty) DiscountLoyaltyCard(None) else DrinksLoyaltyCard(None)
    } yield newCard
  }

  def applyForDiscountLoyaltyCard(): Either[POSError, LoyaltyCard] = {
    for{
      _ <- isValidAge()
      _ <- hasLoyaltyCard()
      _ <- hasMadeMinPurchases()
      _ <- hasSpentMinTotal()
      newCard = DiscountLoyaltyCard(None)
    } yield newCard
  }

  def isValidAge(): Either[POSError, Boolean] = {
    ???
  }

  def hasLoyaltyCard(): Either[POSError, Boolean] = {
    currentLoyaltyCard match {
      case Some(card) => Left(POSError.AlreadyHasCard("You already have a loyalty card"))
      case None => Right(true)
    }
  }

  def hasMadeMinPurchases(): Either[POSError, Boolean] = {
    ???
  }

  def hasSpentMinTotal(): Either[POSError, Boolean] = {
    ???
  }

  def removeCurrentLoyaltyCard():Either[POSError, Boolean] = {
    ???
  }







}
