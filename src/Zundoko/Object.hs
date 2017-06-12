{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.Trans.State.Strict
 import Control.Monad.Trans.Maybe
 import Control.Monad.Trans
 import Data.Functor.Identity
 import Data.Proxy
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = streamObj $ state random

 zundokoStr :: (Monad m) => Object ((->) Int) m -> Object ((->) Bool) m
 zundokoStr = streamObj $ do
  n <- await
  return $ (n `mod` 2) == 1
 
 zundokoMtr :: (Monad m) => Object ((->) Bool) m -> Int -> Object ((->) Bool) (MaybeT m)
 zundokoMtr = streamObj2 $ do
  a <- awaitOn $ lift . lift
  case a of
   False ->
    lift $ modify (1 +)
   True ->
    lift $ do
     n <- get
     case 4 <= n of
      False ->
       modify $ const 0
      True ->
       lift $ nothing
  return a

 foldStream :: r -> (a -> r -> r) -> Object ((->) a) (MaybeT Identity) -> r
 foldStream x f obj = case obj @- id of
  MaybeT (Identity Nothing) -> x
  MaybeT (Identity (Just (a, obj'))) -> f a $ foldStream x f obj'

 streamObj :: (Monad m) => StateT s m a -> s -> Object ((->) a) m
 streamObj s = stateful $ flip fmap s

 streamObj2 :: (Monad m) => StateT s1 (StateT s2 m) a -> s1 -> s2 -> Object ((->) a) m
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (Object ((->) a) m) m a
 await = StateT (@- id)

 awaitOn :: (forall x. m x -> n x) -> StateT (Object ((->) a) m) n a
 awaitOn f = StateT $ f . (@- id)

 nothing :: (Monad m) => MaybeT m a
 nothing = MaybeT $ return Nothing
