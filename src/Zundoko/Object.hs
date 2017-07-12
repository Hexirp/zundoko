{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Zundoko.Object where
 import System.Random (RandomGen, Random, random)
 import Control.Object
 import Control.Monad.Skeleton
 import Data.Functor.Request
 import Control.Monad.Trans.State.Strict
 import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
 import Control.Monad.Trans
 import Data.Functor.Identity (Identity(Identity))
 import Control.Arrow ((>>>), first, second)
 import Control.Applicative (empty)
 import Data.Functor (void)
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> StrObj m r
 randGen = streamObj $ state random

 zundokoStr :: Functor m => StrObj m Int -> StrObj m Bool
 zundokoStr = mapStrObj $ (1 ==) . (`mod` 2)
 
 zundokoMtr :: Monad m => StrObj m Bool -> StrObj (MaybeT m) Bool
 zundokoMtr o = zundokoMtr' o 0

 zundokoMtr' :: Monad m => StrObj m Bool -> Int -> StrObj (MaybeT m) Bool
 zundokoMtr' = streamObj2 $ do
  a <- awaitOn $ lift . lift
  case a of
   False ->
    lift $ do
     modify (1 +)
     return a
   True ->
    lift $ do
     n <- get
     case 4 <= n of
      False -> do
       put 0
       return a
      True ->
       lift $ empty
 
 zundokoTrans :: StrObj (MaybeT Identity) Bool -> StrObj (Skeleton Zundoko) ()
 zundokoTrans = liftStr zundokoRun

 zundokoRun
  :: MaybeT Identity (Bool, StrObj (MaybeT Identity) Bool)
  -> Skeleton Zundoko ((), StrObj (MaybeT Identity) Bool)
 zundokoRun = \case
  MaybeT (Identity Nothing) -> doko >> kiyoshi
  MaybeT (Identity (Just (False, o))) -> zun >> zundokoRun (pullStrObj o)
  MaybeT (Identity (Just (True, o))) -> doko >> zundokoRun (pullStrObj o)
 
 interpretZundoko :: Skeleton Zundoko a -> MaybeT IO a
 interpretZundoko = debone >>> \case
  Return a -> return a
  Zun :>>= k -> do
   a <- lift $ putStrLn "zun"
   interpretZundoko $ k a
  Doko :>>= k -> do
   a <- lift $ putStrLn "doko"
   interpretZundoko $ k a
  Kiyoshi :>>= k -> do
   lift $ putStrLn "kiyoshi"
   a <- empty
   interpretZundoko $ k a

 zundokoInterpreter :: Object (Skeleton Zundoko) (MaybeT IO)
 zundokoInterpreter = liftO interpretZundoko

 zundokoInp :: Object f (Skeleton Zundoko) -> Object f (MaybeT IO)
 zundokoInp = (@>>@ zundokoInterpreter)

 data Zundoko tag where
  Zun :: Zundoko ()
  Doko :: Zundoko ()
  Kiyoshi :: Zundoko a
 
 zun :: Skeleton Zundoko ()
 zun = bone Zun

 doko :: Skeleton Zundoko ()
 doko = bone Doko

 kiyoshi :: Skeleton Zundoko a
 kiyoshi = bone Kiyoshi

 type StrObj m a = Object (Request () a) m

 pullStrObj :: StrObj m a -> m (a, StrObj m a)
 pullStrObj = (@- request ())

 foldStrObj :: Functor f => (f (a, r) -> r) -> StrObj f a -> r
 foldStrObj f o = f $ fmap (second $ foldStrObj f) $ pullStrObj o

 foldListObj :: r -> (a -> r -> r) -> StrObj (MaybeT Identity) a -> r
 foldListObj x f = foldStrObj $ \case
  MaybeT (Identity Nothing) -> x
  MaybeT (Identity (Just (a, r))) -> f a r
 
 mapStrObj :: Functor f => (a -> b) -> StrObj f a -> StrObj f b
 mapStrObj f o = Object $ \(Request _ g) -> fmap (first g . f') $ pullStrObj o
  where
   f' (a, o') = (f a, mapStrObj f o')

 liftStr
  :: Monad n
  => (m (a, StrObj m a) -> n (b, StrObj m a))
  -> StrObj m a -> StrObj n b
 liftStr = streamObj . awaitOn

 streamObj :: Monad m => StateT s m a -> s -> StrObj m a
 streamObj s = stateful $ \(Request _ f) -> fmap f s

 streamObj2 :: Monad m => StateT s1 (StateT s2 m) a -> s1 -> s2 -> StrObj m a
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (StrObj m a) m a
 await = awaitOn id

 awaitOn :: (m (a, StrObj m a) -> n (b, StrObj m a)) -> StateT (StrObj m a) n b
 awaitOn f = StateT $ f . pullStrObj

 -- MaybeT

 voidMaybeT :: Functor f => MaybeT f a -> f ()
 voidMaybeT = void . runMaybeT
