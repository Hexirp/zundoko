module Object where
 import System.Random
 import Zundoko.Object
 import Data.Bool (bool)
 import Prelude

 zundoko :: IO ()
 zundoko = do
  g <- getStdGen
  runZundoko $ zundokoMtr (zundokoStr $ randGen g) 0
 
 runZundoko = foldListObj
  (putStrLn "Doko" >> putStrLn "Kiyoshi")
  (bool (putStrLn "Zun" >>) (putStrLn "Doko" >>))
