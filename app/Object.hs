module Object where
 import System.Random
 import Zundoko.Object
 import Prelude

 main :: IO ()
 main = do
  g <- getStdGen
  voidMaybeT $ pullStrObj $ zundokoInp $ zundokoTrans $ zundokoMtr $ zundokoStr $ randGen g
