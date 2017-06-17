{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.Trans.State.Strict
 import Control.Monad.Trans.Maybe
 import Control.Monad.Trans
 import Data.Functor.Identity
 import Control.Arrow (first, second)
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> StrObj m r
 randGen = streamObj $ state random

 zundokoStr :: Functor m => StrObj m Int -> StrObj m Bool
 zundokoStr = mapStrObj $ (1 ==) . (`mod` 2)
 
 zundokoMtr :: Monad m => StrObj m Bool -> StrObj (MaybeT m) Bool
 zundokoMtr o = zundokoMtr' o 0

 zundokoMtr' :: Monad m => StrObj m Bool -> Int -> StrObj (MaybeT m) Bool
 zundokoMtr' = streamObj2 $ do
  a <- awaitOn $ lift . lift
  case a of
   False ->
    lift $ do
     modify (1 +)
     return a
   True ->
    lift $ do
     n <- get
     case 4 <= n of
      False -> do
       put 0
       return a
      True ->
       lift $ nothing

 type StrObj m a = Object ((->) a) m

 foldStrObj :: Functor f => (f (a, r) -> r) -> StrObj f a -> r
 foldStrObj f o = f $ fmap (second $ foldStrObj f) $ o @- id

 foldListObj :: r -> (a -> r -> r) -> StrObj (MaybeT Identity) a -> r
 foldListObj x f = foldStrObj $ \case
  MaybeT (Identity Nothing) -> x
  MaybeT (Identity (Just (a, r))) -> f a r
 
 mapStrObj :: Functor f => (a -> b) -> StrObj f a -> StrObj f b
 mapStrObj f o = Object $ \g -> fmap (first g . f') (o @- id)
  where
   f' (a, o') = (f a, mapStrObj f o')

 streamObj :: Monad m => StateT s m a -> s -> StrObj m a
 streamObj s = stateful $ flip fmap s

 streamObj2 :: Monad m => StateT s1 (StateT s2 m) a -> s1 -> s2 -> StrObj m a
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (StrObj m a) m a
 await = awaitOn id

 awaitOn :: (m (a, StrObj m a) -> n (b, StrObj m a)) -> StateT (StrObj m a) n b
 awaitOn f = StateT $ f . (@- id)

 nothing :: Monad m => MaybeT m a
 nothing = MaybeT $ return Nothing
