module Restaurant.TabSpec where

import Test.Hspec
import CQRS.Test
import Restaurant.Tab
import Data.UUID
import System.Random

spec :: Spec
spec = do

  testId <- runIO (randomIO :: IO UUID)
  let testTable = 1
  let testWaiter = "Derek"
  let testDrink1 = OrderedItem 0 "tequila" True 5
  let testDrink2 = OrderedItem 1 "vodk" True 5
  let testFood1 = OrderedItem 2 "pancake" False 5.3
  let testFood2 = OrderedItem 3 "hotdog" False 5.2

  describe "restaurant tab system" $ do
    it "can open a new tab" $ do
      testThat $ do
        when_ $ OpenTab testId testTable testWaiter
        then_ $ TabOpened testId testTable testWaiter

    it "can not order without unopened tab" $ do
      testThat $ do
        when_ $ PlaceOrder testId [ testDrink1]
        then_ TabNotOpen

    it "can place drinks order" $ do
      testThat $ do
        given_ $ TabOpened testId testTable testWaiter
        when_ $ PlaceOrder testId [testDrink1, testDrink2]
        then_ $ DrinksOrdered testId testDrink1
        then_ $ DrinksOrdered testId testDrink2

    it "can place food order" $ do
      testThat $ do
        given_ $ TabOpened testId testTable testWaiter
        when_ $ PlaceOrder testId [testFood1, testFood1]
        then_ $ FoodOrdered testId testFood1
        then_ $ FoodOrdered testId testFood1

    it "can place food and drink order" $ do
      testThat $ do
        given_ $ TabOpened testId testTable testWaiter
        when_ $ PlaceOrder testId [testFood1, testDrink1]
        then_ $ DrinksOrdered testId testDrink1
        then_ $ FoodOrdered testId testFood1
