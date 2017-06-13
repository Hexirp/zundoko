module Object where
 import System.Random
 import Zundoko.Object
 import Prelude

 zundoko :: IO ()
 zundoko = do
  g <- getStdGen
  let fol x = case x of {
   False -> (putStrLn "Zun" >>);
   True -> (putStrLn "Doko" >>)}
  foldStream (putStrLn "Doko" >> putStrLn "Kiyoshi") fol $ zundokoMtr (zundokoStr $ randGen g) 0
