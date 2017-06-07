{-# LANGUAGE LambdaCase #-}

module Zundoko
 ( zundoko
 , Zundoko(Zun, Doko, Kiyoshi)
 ) where
 import Prelude
 import Control.Category ((>>>))

 zundoko :: [Int] -> Zundoko
 zundoko = runZundokoS . toZundokoS

 data Zundoko = Zun Zundoko | Doko Zundoko | Kiyoshi
 
 runZundokoS :: ZundokoS -> Zundoko
 runZundokoS = undefined
 
 toZundokoS :: [Int] -> ZundokoS
 toZundokoS = foldr zun eoz
  where
   eoz = ZunS $ ZunS $ ZunS $ ZunS dos
   dos = DokoS dos
   zun = (`mod` 2) >>> (== 1) >>> \case
    False -> ZunS
    True -> DokoS
 
 data ZundokoS = ZunS ZundokoS | DokoS ZundokoS
