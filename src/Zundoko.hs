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
 runZundokoS = zun Zun0
  where
   zun = \case
    Zun0 -> \case
     ZunS x -> Zun $ zun Zun1 x
     DokoS x -> Doko $ zun Zun0 x
    Zun1 -> \case
     ZunS x -> Zun $ zun Zun2 x
     DokoS x -> Doko $ zun Zun0 x
    Zun2 -> \case
     ZunS x -> Zun $ zun Zun3 x
     DokoS x -> Doko $ zun Zun0 x
    Zun3 -> \case
     ZunS x -> Zun $ zun ZunM x
     DokoS x -> Doko $ zun Zun0 x
    ZunM -> \case
     ZunS x -> Zun $ zun ZunM x
     DokoS x -> Doko $ Kiyoshi
 
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
