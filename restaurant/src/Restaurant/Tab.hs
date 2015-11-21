{-#LANGUAGE TypeFamilies #-}
module Restaurant.Tab where

import CQRS.Aggregate
import Data.UUID

data Tab = Tab
  {
  }


instance Aggregate Tab where
  data Command Tab 
    = OpenTab UUID Int String
    deriving (Eq, Show)

  data Event Tab
    = TabOpened UUID Int String
    deriving (Eq, Show)

  data Error Tab
    = NotImplemented
    deriving (Eq, Show)

  apply t e  = t
  process t c = Left NotImplemented

