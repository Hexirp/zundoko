module Main where
 import System.Random
 import Zundoko.Object

 main :: IO ()
 main = do
  g <- getStdGen
  voidMaybeT $ pullStrObj $ zundokoInp $ zundokoTrans $ zundokoMtr $ zundokoStr $ randGen g
