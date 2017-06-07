{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.State.Strict
 import Prelude

 data Rand ret where
  Next :: (Random a) => Rand a

 randGen :: (Monad m, RandomGen a) => a -> Object Rand m
 randGen = stateful $ \case
  Next -> state random
