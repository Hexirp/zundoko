{-# LANGUAGE LambdaCase #-}

module Zundoko
 ( zdk
 , Zundoko(Zun, Doko, Kiyoshi)
 ) where
 import Prelude

 zdk :: [Int] -> Zundoko
 zdk = toZundoko . toZundokoS

 data Zundoko = Zun Zundoko | Doko Zundoko | Kiyoshi
 
 toZundoko :: ZundokoS -> Zundoko
 toZundoko x0 = zun x0 Zun0
  where
   zun :: ZundokoS -> ZundokoC -> Zundoko
   zun x s = case x of
    ZunS x' -> Zun $ zun x' (inc s)
    DokoS x' -> Doko $ dok x' s
   inc :: ZundokoC -> ZundokoC
   inc s = case s of
    Zun0 -> Zun1
    Zun1 -> Zun2
    Zun2 -> Zun3
    Zun3 -> ZunM
    ZunM -> ZunM
   dok :: ZundokoS -> ZundokoC -> Zundoko
   dok x s = case s of
    ZunM -> Kiyoshi
    _ -> zun x Zun0
 
 data ZundokoC = Zun0 | Zun1 | Zun2 | Zun3 | ZunM
 
 toZundokoS :: [Int] -> ZundokoS
 toZundokoS = foldr zun eoz
  where
   eoz :: ZundokoS
   eoz = ZunS $ ZunS $ ZunS $ ZunS dos
   dos = DokoS dos
   zun :: Int -> ZundokoS -> ZundokoS
   zun x s = case x `mod` 2 == 1 of
    False -> ZunS s
    True -> DokoS s
 
 data ZundokoS = ZunS ZundokoS | DokoS ZundokoS
