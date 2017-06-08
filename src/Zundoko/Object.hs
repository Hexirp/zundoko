{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.State.Strict
 import Control.Monad.Trans.Maybe
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = stateful $ flip fmap $ state random

 zundokoStr :: (Monad m) => Object ((->) Int) m -> Object ((->) Bool) m
 zundokoStr = stateful $ flip fmap $ do
  obj <- get
  lift $ do
   (n, _) <- obj @- id
   return $ (n `mod` 2) == 1
 
 zundokoMtr :: (Monad m) => Int -> Object ((->) Bool) m -> Object ((->) Bool) (MaybeT m)
 zundokoMtr = curry $ stateful $ flip fmap $ s
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
   -- :: MaybeT m a
   nothing = MaybeT $ return Nothing
