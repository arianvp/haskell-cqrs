module Restaurant.TabSpec where

import Test.Hspec
import CQRS.Test
import Restaurant.Tab
import Data.UUID.V4

spec :: Spec
spec = do

  testId <- liftIO
  let testTable = 1
  let testWaiter = "Derek"

  describe "restaurant tab system" $ do
    it "can open a new tab" $ do
      when_ $ OpenTab testId testTable testWaiter
      then_ $ TabOpened testId testTable testWaiter
      
