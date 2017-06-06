{-# LANGUAGE LambdaCase #-}

module Main where
 import Prelude
 import System.Random
 import Lib

 main :: IO ()
 main = do
  putStrLn ""
  rs <- randStream
  runZundoko $ zundoko rs

 randStream :: IO [Int]
 randStream = do
  gen <- getStdGen
  return $ randoms gen

 runZundoko :: Zundoko -> IO ()
 runZundoko = \case
  Zun x -> do
   putStrLn "ズン"
   runZundoko x
  Doko x -> do
   putStrLn "ドコ"
   runZundoko x
  Kiyoshi -> do
   putStrLn "キ・ヨ・シ！"
