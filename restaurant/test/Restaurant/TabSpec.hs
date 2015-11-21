module Restaurant.TabSpec where

import Test.Hspec
import CQRS.Test
import Restaurant.Tab
import Data.UUID
import System.Random

spec :: Spec
spec = do

  testId <- runIO randomIO
  let testId = 3
  let testTable = 1
  let testWaiter = "Derek"
  let testDrink1 = undefined

  describe "restaurant tab system" $ do
    it "can open a new tab" $ do
      testThat $ do
        when_ $ OpenTab testId testTable testWaiter
        then_ $ TabOpened testId testTable testWaiter

  describe "can not order with unopened tab" $ do
    testThat $ do
      when_ $ PlaceOrder testId [testDrink1]
      thenFailWith_ TabNotOpen

  describe "can place drinks order" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      when_  $ PlaceOrder testId [testDrink1, testDrink2]
      then_  $ DrinksOrdered testId [testDrink1, testDrink2]

  describe "can place food order" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      when_  $ PlaceOrder testId
      then_  $ FoodOrdered testId [testFood1, testFood2]
  
  describe "can place food and drink order" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      when_  $ PlaceOrder testId [testFood1, testDrink1]
      then_  $ DrinksOrdered testId [testDrink1]
      then_  $ FoodOrdered testId [testFood1]


  describe "ordered drinks can be served" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      given_ $ DrinksOdered testId [testDrink1, testDrink2]
      when_  $ ServeDrinks testId [testDrink1, testDrink2]

  describe "can not serve an unordered drink" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      given_ $ DrinksOrdered testId [testDrink1]
      when_  $ ServeDrinks testId [testDrink2]
      thenFailWith DrinkNotOutstanding

  describe "can not serve an ordered drink twice" $ do
    testThat $ do
      given_ $ TabOpened testId testTable testWaiter
      given_ $ DrinksOrdered testId [testDrink1]
      given_ $ DrinksServed testId [testDrink]
      when_  $ ServeDrinks testId [testDrink1]
      thenFailWith_ $ DrinksNotOutstanding



