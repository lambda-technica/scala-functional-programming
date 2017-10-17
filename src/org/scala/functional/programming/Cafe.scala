package org.scala.functional.programming


case class CreditCard(balance: Double)

case class Coffee(price: Double)

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Cannot combine charges")
  }
}

/**
  * Coffee shop class
  */
class Cafe {

  val price = 10D

  /**
    * Buy single coffee
    *
    * @param cc Credit card to process
    * @return cup and charge tuple
    */
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee(price)

    (cup, Charge(cc, cup.price))
  }

  /**
    * Buy multiple coffees
    *
    * @param cc Credit card to be charges
    * @param n number of coffees to order
    * @return coffees and charges tuple
    */
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))

    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))

  }
}

object Main extends App {
  val cc = CreditCard(100)
  val coffeeShop = new Cafe()

  //buy one coffee
  val (cup,charges) = coffeeShop.buyCoffee(cc)

  print( s"Cup price is ${cup.price} , charges balance ${charges.amount}")

  //buy multiple coffees
  val (coffees,multicharges) = coffeeShop.buyCoffees(cc, 5)

  coffees.foreach( coffee =>  println( s"Bought coffee: ${coffee.price} "))

  println(s"Charges amount: ${multicharges.amount}")



}
