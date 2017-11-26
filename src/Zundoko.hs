{-# LANGUAGE LambdaCase #-}

module Zundoko
 ( zdk
 , Zundoko(Zun, Doko, Kiyoshi)
 ) where
 import Prelude

 zdk :: [Int] -> Zundoko
 zdk = toZundoko . toZundokoS

 data Zundoko = Zun Zundoko | Doko Zundoko | Kiyoshi

 zundoko :: (r -> r) -> (r -> r) -> r -> Zundoko -> r
 zundoko z d k = go where
  go (Zun x) = z (go x)
  go (Doko x) = d (go x)
  go Kiyoshi = k
 
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

 zundokoS :: (r -> r) -> (r -> r) -> ZundokoS -> r
 zundokoS z d = go where
  go (ZunS x) = z (go x)
  go (DokoS x) = d (go x)
