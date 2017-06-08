{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.State.Strict
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = stateful $ (<$> state random)
