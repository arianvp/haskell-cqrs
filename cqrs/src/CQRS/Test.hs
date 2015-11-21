{-#LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module CQRS.Test where
import CQRS.Aggregate
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.IO.Class
import Test.HUnit
import Data.List hiding (init)


data Environment a = Environment
  { _commands   :: [Command a]
  , _aggregate :: a
  }

data Result = Result

type AggregateSpec a = StateT (Environment a) IO ()

given_ :: Aggregate a
       => Event a
       -> AggregateSpec a
given_ e = do
  env <- get
  put env { _aggregate = _aggregate env `apply` e }

when_ :: Aggregate a
      => Command a
      -> AggregateSpec a
when_ c = do
  env <- get
  put env { _commands = _commands env ++ [c] }

then_ :: (Aggregate a, Eq (Event a), Show (Event a), Show (Error a))
     => Event a
     -> AggregateSpec a
then_ e = do
  env <- get
  case uncons (_commands env) of
    Just (c,cs) ->
      case process (_aggregate env) c of
        Left err -> liftIO $ assertFailure $  "expected event: " ++ show e ++ "\nbut got error: " ++ show err
        Right e2 ->
          if e == e2
            then put (env { _commands = cs })
            else liftIO $ assertFailure $ "expected event:" ++ show e ++ "\nbut got:" ++ show e2
    Nothing ->
      liftIO $ assertFailure $ "expected event: "  ++ show e ++ "\nbut it was never emitted"


thenFailWith_ :: (Aggregate a, Eq (Event a), Show (Event a), Show (Error a), Eq (Error a))
     => Error a
     -> AggregateSpec a
thenFailWith_ err = do
  env <- get
  case uncons (_commands env) of
    Just (c,cs) ->
      case process (_aggregate env) c of
        Right e -> liftIO $ assertFailure $  "expected error: " ++ show err ++ "\nbut got event: " ++ show e
        Left e2 ->
          if err == e2
            then put (env { _commands = cs })
            else liftIO $ assertFailure $ "expected error:" ++ show err ++ "\nbut got:" ++ show e2
    Nothing ->
      liftIO $ assertFailure $ "expected error: "  ++ show err ++ "\nbut it was never emitted"

  
testThat :: Aggregate a => AggregateSpec a -> Assertion
testThat s = evalStateT s (Environment [] initial)
    

