{-# LANGUAGE GADTs #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Prelude

 data Rand ret where
  Next :: (Random a) => Rand a

 randGen :: (Monad m, RandomGen a) => a -> Object Rand m
 randGen x = Object obj
  where
   obj :: Monad m => Rand a -> m (a, Object Rand m)
   obj Next = do
    let (a, g) = random x
    return $ (a, randGen g)
