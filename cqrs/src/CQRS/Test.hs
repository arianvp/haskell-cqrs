{-#LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module CQRS.Test where
import CQRS.Aggregate
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.IO.Class
import Test.HUnit
import Data.List hiding (init)
import Data.Monoid
import Data.Default
import Data.Sequence


data Environment a = Environment
  { _events    :: Seq (Event a)
  , _aggregate :: a
  }


type AggregateSpec a = StateT (Environment a) IO ()

given_ :: Aggregate a
       => Event a
       -> AggregateSpec a
given_ e =
  modify (\env ->
    env { _aggregate = _aggregate env `apply` e
        })

when_ :: DomainLogic a
      => Command a
      -> AggregateSpec a
when_ c = 
  modify (\env ->
    env { _events = _events env <> process (_aggregate env) c })

  
  
then_ :: (DomainLogic a, Eq (Event a), Show (Event a))
      => Event a
      -> AggregateSpec a
then_ e = do
  env <- get
  case viewl (_events env) of
    (e2 :< es) ->
      if e == e2
        then put (env { _events = es })
        else liftIO (assertFailure ("expected event:" ++ show e ++ "\n but got:" ++ show e2 ))
    EmptyL ->
      liftIO (assertFailure ("expected event: " ++ show e ++ "\n but it was never emitted"))

testThat :: DomainLogic a => AggregateSpec a -> Assertion
testThat s = evalStateT s (Environment mempty def)
    

