module Zundoko
 ( zundoko
 , Zundoko(Zun, Doko, Kiyoshi)
 ) where
 import Prelude

 zundoko :: [Int] -> Zundoko
 zundoko = runZundokoS . toZundokoS

 data Zundoko = Zun Zundoko | Doko Zundoko | Kiyoshi
 
 runZundokoS :: ZundokoS -> Zundoko
 runZundokoS = undefined
 
 toZundokoS :: [Int] -> ZundokoS
 toZundokoS = undefined
 
 data ZundokoS = ZunS ZundokoS | DokoS ZundokoS
