{-#LANGUAGE TypeFamilies #-}
module CQRS.Aggregate where

class Aggregate a where
  data Command a
  data Event a
  data Error a
