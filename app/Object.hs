module Object where
 import System.Random
 import Zundoko.Object
 import Prelude

 main :: IO ()
 main = do
  g <- getStdGen
  effMaybe $ pull $ inp $ trans $ mtr $ str $ rand g
