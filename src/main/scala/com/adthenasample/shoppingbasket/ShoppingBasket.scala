package com.adthenasample.shoppingbasket

/**
Write a program driven by unit tests that can price a basket of goods taking into account some special offers.
The goods that can be purchased, together with their normal prices are:
● Soup – 65p per tin
● Bread – 80p per loaf
● Milk – £1.30 per bottle
● Apples – £1.00 per bag
Current special offers
● Apples have a 10% discount off their normal price this week
● Buy 2 tins of soup and get a loaf of bread for half price
The program should accept a list of items in the basket and output the subtotal, the special offer discounts and the final price.
Input should be via the command line in the form PriceBasket item1 item2 item3 ...
For example
PriceBasket Apples Milk Bread
Output should be to the console, for example:
Subtotal: £3.10
Apples 10% off: 10p
Total price: £3.00
If no special offers are applicable the code should output:
Subtotal: £1.30
(No offers available)
Total price: £1.30
*/

object ShoppingBasket {
  val soupCost = 65
  val breadCost = 80
  val milkCost = 130
  val applesCost = 100


  /**
   * Calculate the discount amount of item.
   */
  def discountAmt(price: Int, discount: Int): Double = {
    price * discount / 100
  }

  /**
   * Calculate the price of item after discount.
   */
  def discountedPrice(price: Int, discount: Int): Double = {
    price - discountAmt(price, discount)
  }

  /**
   * Calculate the price of bread after discount.
   */
  def discountedBreadCost(soupCount: Int, breadCount: Int): Double = {
    val discountedBreadCount = soupCount / 2
    if(discountedBreadCount < breadCount)
      (discountedPrice(breadCost, 50) * discountedBreadCount) + ((breadCount - discountedBreadCount) * breadCost)
    else
      (discountedPrice(breadCost, 50) * breadCount)
  }

  /**
   * Calculate the discount amount of bread.
   */

  def breadDiscountAmt(soupCount: Int, breadCount: Int): Double = {
    val discountedBreadCount = soupCount / 2
    if(discountedBreadCount < breadCount)
      (discountAmt(breadCost, 50) * discountedBreadCount)
    else
      (discountAmt(breadCost, 50) * breadCount)
  }

  /**
   * Calculate the sub total i.e. without discount.
   */

  def subTotal(soupCount: Int, breadCount: Int, milkCount: Int, applesCount: Int): Double = {
    (soupCost * soupCount) + (breadCost * breadCount) + (milkCost * milkCount) + (applesCost * applesCount)
  }

  /**
   * Calculate the total cost of all items after discount.
   */
  def totalCost(soupCount: Int, breadCount: Int, milkCount: Int, applesCount: Int): Double = {
    (soupCost * soupCount) + discountedBreadCost(soupCount, breadCount) + (milkCost * milkCount) + (discountedPrice(applesCost, 10) * applesCount)
  }

  /*
   * Sample Inputs:
   * Input: PriceBasket Apples Milk Bread
   * Output:
    Subtotal: £3.10
    Apples 10% off: 10p
    Total price: £3.00
   *
   *
   * PriceBasket Apples Milk Bread Soup Soup
   * Subtotal: £4.40
    Apples 10% off: 10p
    Breads 50% off: 40p
    Total price: £3.90
   *
   * Input: PriceBasket Apples Milk Bread Soup Soup Bread
   * Output:
    Subtotal: £5.20
    Apples 10% off: 10p
    Breads 50% off: 40p
    Total price: £4.70

   * Input: PriceBasket Apples Milk Bread Soup Soup Soup Bread
   * Output:
    Subtotal: £5.85
    Apples 10% off: 10p
    Breads 50% off: 40p
    Total price: £5.35

   * Input: PriceBasket Apples Milk Bread Soup Soup Soup Soup Bread
   * Output:
    Subtotal: £6.50
    Apples 10% off: 10p
    Breads 50% off: 80p
    Total price: £5.60

   * Input: PriceBasket Apples Milk Bread Apples Apples Apples Apples Apples Apples Apples Apples Apples
   * Output:
    Subtotal: £12.10
    Apples 10% off: £1.00
    Total price: £11.10

   */

  def main(args: Array[String]): Unit = {

    var soupCount: Int = 0
    var breadCount: Int = 0
    var milkCount: Int = 0
    var applesCount: Int = 0

    val input = scala.io.StdIn.readLine()

    input.split(" ").foreach(_ match {
      case "Soup" => soupCount += 1
      case "Bread" => breadCount += 1
      case "Milk" => milkCount += 1
      case "Apples" => applesCount += 1
      case _ =>
    } )

    var noDiscount = true

    println("Subtotal: £%.2f".format(subTotal(soupCount, breadCount, milkCount, applesCount) / 100))

    val applesDiscount = discountAmt(applesCost, 10) * applesCount
    if(applesDiscount != 0)
    {
      if(applesDiscount < 100)
        println("Apples 10% off: " + applesDiscount.toInt + "p")
      else
        println("Apples 10% off: " + "£%.2f".format(applesDiscount / 100))
      noDiscount = false
    }

    val breadDiscount = breadDiscountAmt(soupCount, breadCount)
    if(breadDiscount != 0)
    {
      if(breadDiscount < 100)
        println("Breads 50% off: " + breadDiscount.toInt + "p")
      else
        println("Breads 50% off: " + "£%.2f".format(breadDiscount / 100d))
      noDiscount = false
    }

    if(noDiscount)
      println("(No offers available)")

    println("Total price: £%.2f".format(totalCost(soupCount, breadCount, milkCount, applesCount) / 100))

  }
}