package com.adthenasample.shoppingbasket

import org.scalatest.FunSuite

class ShoppingBasketTest extends FunSuite {

  val soupCounForTest = 1
  val breadCountForTest=1
  val milkCountForTest=1
  val applesCountForTest=1

  test("Check actual vs expected for all items bought as individual") {

    val subtotalActualCostSingle = ShoppingBasket.subTotal(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val subtotalExpectedCostSingle = 3.75
    assert(subtotalActualCostSingle === subtotalExpectedCostSingle, "Values Match on SubTotal for all items bought as individual")

    val totalActualCostSingle = ShoppingBasket.totalCost(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val totalExpectedCostSingle = 3.65
    assert(totalActualCostSingle === totalExpectedCostSingle, "Values Match on Total for all items bought as individual")

  }

  test("Check actual vs expected for all items bought as individual excluding apples") {

    val soupCounForTest = 1
    val breadCountForTest=1
    val milkCountForTest=1
    val applesCountForTest=0

    val subtotalActualCostNoApple = ShoppingBasket.subTotal(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val subtotalExpectedCostNoApple = 2.75
    assert(subtotalActualCostNoApple === subtotalExpectedCostNoApple, "Values Match on SubTotal Without Apple")
  }


  test("Check actual vs expected with Bread discount on Soup without Apples") {

    val soupCounForTest = 2
    val breadCountForTest=1
    val milkCountForTest=1
    val applesCountForTest=0

    val subActBreadDiscNoApple = ShoppingBasket.subTotal(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  subExpBreadDiscNoApple = 3.40
    assert(subActBreadDiscNoApple === subExpBreadDiscNoApple, "Values Match on SubTotal for Bread discount on Soup without Apples")

    val totActBreadDiscNoApple = ShoppingBasket.totalCost(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  totExpBreadDiscNoApple = 3.0
    assert(totActBreadDiscNoApple === totExpBreadDiscNoApple, "Values Match on Total for Bread discount on Soup without Apples")
  }


  test("Check actual vs expected with Bread discount on Soup with Apples") {

    val soupCounForTest = 2
    val breadCountForTest=1
    val milkCountForTest=1
    val applesCountForTest=1

    val subActBreadDiscWithApple = ShoppingBasket.subTotal(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  subExpBreadDiscWithApple = 4.40
    assert(subActBreadDiscWithApple === subExpBreadDiscWithApple, "Values Match on SubTotal with Bread discount on Soup with Apples")

    val totActBreadDiscWithApple = ShoppingBasket.totalCost(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  totExpBreadDiscWithApple = 3.9
    assert(totActBreadDiscWithApple === totExpBreadDiscWithApple, "Values Match on Total with Bread discount on Soup with Apples")

  }

  test("Check actual vs expected Multiple Items") {

    val soupCounForTest = 2
    val breadCountForTest=2
    val milkCountForTest=2
    val applesCountForTest=10

    val subActMultiple = ShoppingBasket.subTotal(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  subExpMultiple = 15.50
    assert(subActMultiple === subExpMultiple, "Values Match on SubTotal for Multiple Items")

    val totActMultiple = ShoppingBasket.totalCost(soupCounForTest, breadCountForTest, milkCountForTest, applesCountForTest) / 100
    val  totExpMultiple = 14.50
    assert(totActMultiple === totActMultiple, "Values Match on Total for Multiple Items")

  }

}

