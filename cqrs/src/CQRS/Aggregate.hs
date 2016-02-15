{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ConstraintKinds #-}
module CQRS.Aggregate where
import Data.Default
import Data.Sequence

class Default a => DomainLogic a where
  data Command a
  data Event a
  process :: a -> Command a -> Seq (Event a)

class (DomainLogic a, Default a) => Projection a r  where
  apply :: r -> Event a -> r

type Aggregate a = Projection a a


