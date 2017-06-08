{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.State.Strict
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = stateful $ flip fmap $ state random

 zundokoStr :: (Monad m) => Object ((->) Int) m -> Object ((->) Bool) m
 zundokoStr = stateful $ flip fmap $ do
  obj <- get
  lift $ do
   (n, _) <- obj @- id
   return $ (n `mod` 2) == 1
