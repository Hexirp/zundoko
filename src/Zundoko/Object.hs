module Zundoko.Object where
 import System.Random
 import Control.Object
 import Control.Monad.Trans.State.Strict
 import Control.Monad.Trans.Maybe
 import Control.Monad.Trans
 import Data.Functor.Identity
 import Prelude

 randGen :: (RandomGen a, Random r, Monad m) => a -> StrObj m r
 randGen = streamObj $ state random

 zundokoStr :: Monad m => StrObj m Int -> StrObj m Bool
 zundokoStr = streamObj $ do
  n <- await
  return $ (n `mod` 2) == 1
 
 zundokoMtr :: Monad m => StrObj m Bool -> Int -> StrObj (MaybeT m) Bool
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

 type StrObj m a = Object ((->) a) m

 foldStream :: r -> (a -> r -> r) -> StrObj (MaybeT Identity) a -> r
 foldStream x f obj = case obj @- id of
  MaybeT (Identity Nothing) -> x
  MaybeT (Identity (Just (a, obj'))) -> f a $ foldStream x f obj'

 streamObj :: Monad m => StateT s m a -> s -> StrObj m a
 streamObj s = stateful $ flip fmap s

 streamObj2 :: Monad m => StateT s1 (StateT s2 m) a -> s1 -> s2 -> StrObj m a
 streamObj2 s a b = streamObj s a @>>@ variable b

 await :: StateT (StrObj m a) m a
 await = awaitOn id

 awaitOn
  :: (m (a, StrObj m a) -> n (b, StrObj m a))
  -> StateT (StrObj m a) n b
 awaitOn f = StateT $ f . (@- id)

 nothing :: Monad m => MaybeT m a
 nothing = MaybeT $ return Nothing
