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
 runZundokoS = flip zun Zun0
  where
   zun = \case
    ZunS x -> Zun . zun x . inc
    DokoS x -> Doko . flip dok x
   inc = \case
    Zun0 -> Zun1
    Zun1 -> Zun2
    Zun2 -> Zun3
    Zun3 -> ZunM
    ZunM -> ZunM
   dok = \case
    ZunM -> const Kiyoshi
    _ -> flip zun Zun0
 
 data ZundokoC = Zun0 | Zun1 | Zun2 | Zun3 | ZunM
 
 toZundokoS :: [Int] -> ZundokoS
 toZundokoS = foldr zun eoz
  where
   eoz = ZunS $ ZunS $ ZunS $ ZunS dos
   dos = DokoS dos
   zun = (`mod` 2) >>> (== 1) >>> \case
    False -> ZunS
    True -> DokoS
 
 data ZundokoS = ZunS ZundokoS | DokoS ZundokoS
