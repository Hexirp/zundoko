module Main where
 import System.Random
 import Zundoko.Object

 main :: IO ()
 main = do
  g <- getStdGen
  effMaybe $ pull $ inp $ trans $ mtr $ str $ rand g
