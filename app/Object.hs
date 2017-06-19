module Object where
 import System.Random
 import Zundoko.Object
 import Data.Bool (bool)
 import Prelude

 main :: IO ()
 main = do
  g <- getStdGen
  runZundoko $ zundokoMtr $ zundokoStr $ randGen g
 
 runZundoko = foldListObj
  (putStrLn "Doko" >> putStrLn "Kiyoshi")
  (bool (putStrLn "Zun" >>) (putStrLn "Doko" >>))