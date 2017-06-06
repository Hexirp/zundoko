module Zundoko
 ( zundoko
 , Zundoko(Zun, Doko, Kiyoshi)
 ) where
 import Prelude

 zundoko :: [Int] -> Zundoko
 zundoko = undefined

 data Zundoko = Zun Zundoko | Doko Zundoko | Kiyoshi
