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

  
  
{- 
 -
 - testThat $ do
 -  when_ (RegisterUser e)
 -  then_ (MailSent e)
 -  then_ (UserRegistered e)
 - testThat $ do
 -  given_ UserRegistered e
 -  when_ (RegisterUser e)
 -  then_ (UserAlreadyRegistered e)
 -}

{-
then_' :: (DomainLogic a, Eq (Event a), Show (Event a)) 
       => Event a
       -> AggregateSpec a
then_' e = do
  env <- get
  case env of
    (Environment []     []     a) ->
      undefined  -- we have no events in the buffer. we should fail
    (Environment (c:cs) []     a) -> do
      let res = process a c
      put $ Environment cs res a
      then_' e
    (Environment []     (e':es) a) -> do
      if e' == e
        then put $ Environment [] es (apply e a)
        else liftIO $ assertFailure $ "expected event:" ++ show e ++ "\nbut got:" ++ show e'
      let a' = apply a e
      put $ Environment [] es a'
    (Environment (c:cs) (e':es) a) -> undefined

then_ :: (DomainLogic a, Eq (Event a), Show (Event a), Show (Error a))
      => Event a
      -> AggregateSpec a
then_ e = do
  env <- get
  case uncons (_commands env) of
    Just (c,cs) ->
      case process (_aggregate env) c of
        Fail err -> liftIO $ assertFailure $  "expected event: " ++ show e ++ "\nbut got error: " ++ show err
        Emit e2 ->
          if e == e2
            then put (env { _commands = cs })
            else liftIO $ assertFailure $ "expected event:" ++ show e ++ "\nbut got:" ++ show e2
    Nothing ->
      liftIO $ assertFailure $ "expected event: "  ++ show e ++ "\nbut it was never emitted"


-} 
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
    

