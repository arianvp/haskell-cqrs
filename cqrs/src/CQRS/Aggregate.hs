{-#LANGUAGE TypeFamilies #-}
module CQRS.Aggregate where

class Aggregate a where
  data Command a
  data Event a
  data Error a

  process :: a -> Command a -> Either (Error a) (Event a)
  apply :: a -> Event a -> a
  
  initial :: a

