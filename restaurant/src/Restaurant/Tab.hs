{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Restaurant.Tab where
import Prelude hiding (filter)
import CQRS.Aggregate
import Data.UUID
import Data.Monoid
import Data.Default
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.List as List

data Tab = Tab
  { isOpen :: Bool
  }

data OrderedItem = OrderedItem
  { menuNumber :: Int
  , description :: String
  , isDrink :: Bool
  ,  price :: Double
  } deriving (Eq, Show)

instance Default Tab where
  def = Tab False

instance DomainLogic Tab where
  data Command Tab 
    = OpenTab UUID Int String
    | PlaceOrder UUID [OrderedItem]
    deriving (Eq, Show)

  data Event Tab
    = TabOpened UUID Int String
    | DrinksOrdered UUID OrderedItem
    | FoodOrdered UUID OrderedItem
    | TabNotOpen
    deriving (Eq, Show)


  process t (OpenTab id talbeNumber waiter) =
    Seq.singleton (TabOpened id talbeNumber waiter)

  process t (PlaceOrder id items) =
    if (not . isOpen) t
      then Seq.singleton TabNotOpen
      else fmap (DrinksOrdered id) drinks <> fmap (FoodOrdered id) food
    where
      drinks = Seq.filter isDrink (Seq.fromList items)
      food   = Seq.filter (not . isDrink) (Seq.fromList items)

        


instance Projection Tab Tab where
  apply t (TabOpened id tableNumber waiter) =
    t { isOpen = True }

