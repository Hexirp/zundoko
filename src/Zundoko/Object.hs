{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.Trans.State.Strict
 import Control.Monad.Trans.Maybe
 import Control.Monad.Trans
 import Data.Proxy
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> Object ((->) r) m
 randGen = streamObj $ state random

 zundokoStr :: (Monad m) => Object ((->) Int) m -> Object ((->) Bool) m
 zundokoStr = streamObj $ do
  n <- await
  return $ (n `mod` 2) == 1
 
 zundokoMtr :: (Monad m) => Object ((->) Bool) m -> Int -> Object ((->) Bool) (MaybeT m)
 zundokoMtr = streamObj2 $ do
  a <- awaitOn $ lift . lift
  case a of
   False ->
    lift $ modify (1 +)
   True ->
    lift $ do
     n <- get
     case 4 <= n of
      False ->
       modify $ const 0
      True ->
       lift $ nothing
  return a
 
 zundokoIO :: Object ((->) Bool) (MaybeT IO) -> Object ((->) ()) (MaybeT IO)
 zundokoIO = streamObj $ do
  a <- await
  case a of
   False ->
    lift $ lift $ putStrLn "zun"
   True ->
    lift $ lift $ putStrLn "doko"

 doStream :: (Monad m) => Object ((->) ()) (MaybeT m) -> Object ((->) ()) (ProxyT m)
 doStream = streamObj $ run
  where
   run :: Monad m => StateT (Object ((->) ()) (MaybeT m)) (ProxyT m) ()
   run = do
    awaitOn $ toP
    run
   toP :: Functor f => MaybeT f a -> ProxyT f a
   toP = ProxyT . (Proxy <$) . runMaybeT

 streamObj :: (Monad m) => StateT s m a -> s -> Object ((->) a) m
 streamObj s = stateful $ flip fmap s

 streamObj2 :: (Monad m) => StateT s1 (StateT s2 m) a -> s1 -> s2 -> Object ((->) a) m
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (Object ((->) a) m) m a
 await = StateT (@- id)

 awaitOn :: (forall x. m x -> n x) -> StateT (Object ((->) a) m) n a
 awaitOn f = StateT $ f . (@- id)

 nothing :: (Monad m) => MaybeT m a
 nothing = MaybeT $ return Nothing

 newtype ProxyT m a = ProxyT { outProxyT :: m (Proxy a) }

 instance (Functor m) => Functor (ProxyT m) where
  fmap f (ProxyT x) = ProxyT $ fmap (fmap f) x

 instance (Applicative m) => Applicative (ProxyT m) where
  pure = ProxyT . pure . pure

  (ProxyT f) <*> (ProxyT x) = ProxyT $ (<*>) <$> f <*> x

 instance (Applicative m) => Monad (ProxyT m) where
  (ProxyT x) >>= f = ProxyT $ Proxy <$ fmap (fmap f) x

 instance MonadTrans ProxyT where
  lift = ProxyT . (Proxy <$)
 
 law1 :: Maybe (Proxy a)
 law1 = outProxyT $ ProxyT (Just Proxy) <* ProxyT Nothing

 law2 :: Maybe (Proxy a)
 law2 = outProxyT $ ProxyT (Just Proxy) >> ProxyT Nothing
