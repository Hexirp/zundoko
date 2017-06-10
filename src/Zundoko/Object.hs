{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.State.Strict
 import Control.Monad.Trans.Maybe
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = streamObj $ state random

 zundokoStr :: (Monad m) => Object ((->) Int) m -> Object ((->) Bool) m
 zundokoStr = streamObj $ do
  n <- await
  return $ (n `mod` 2) == 1
 
 zundokoMtr :: (Monad m) => Int -> Object ((->) Bool) m -> Object ((->) Bool) (MaybeT m)
 zundokoMtr = curry $ streamObj $ s
  where
   -- :: StateT (Int, Object ((->) Bool) (MaybeT m) Bool
   s = do
    (n, obj) <- get
    (b, obj') <- lift $ lift $ obj @- id
    case b of
     False -> put (n + 1, obj')
     True -> if 4 <= n
      then
       lift $ nothing
      else
       put (0, obj')
    return $ b
   
 streamObj :: (Monad m) => StateT s m a -> s -> Object ((->) a) m
 streamObj s = stateful $ flip fmap $ s
 
 streamObj2 :: (Monad m) => StateT s1 (StateT s2 m) a -> s1 -> s2 -> Object ((->) a) m
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (Object ((->) a) m) m a
 await = StateT (@- id)

 awaitOn :: (forall x. m x -> n x) -> StateT (Object ((->) a) m) n a
 awaitOn f = StateT $ f . (@- id)

 nothing :: (Monad m) => MaybeT m a
 nothing = MaybeT $ return Nothing
