{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Cont where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.State

data Result t a =
    Fine a
  | EInt (Int -> t (Result t a) (Result t a))
  | EBool (Bool -> t (Result t a) (Result t a))

newtype VKT m r a = VKT { unVKT :: StateT (r -> VKT m r r) (ContT r m) a }
  deriving(Functor, Applicative, Monad, MonadState (r -> VKT m r r), MonadCont, MonadIO)

runVKT :: (Monad m) => VKT m r r -> m r
runVKT m = runContT (runStateT (unVKT (protect m)) undefined) (return . fst) where
  protect :: VKT m r r -> VKT m r r
  protect m = do
    callCC $ \k -> do
      modify (const k)
      m

raise :: (MonadCont m, MonadState (a1 -> m a1) m) => ((a -> m b) -> a1) -> m a
raise z = callCC $ \k -> do
  err <- get
  err (z k)
  undefined


test1 :: IO ()
test1 = do
  (EInt k) <- runVKT $ do
                  liftIO $ putStrLn "p1"
                  x <- raise EInt
                  liftIO $ putStrLn $ "x=" ++ (show x)
                  return (Fine (42::Int))
  putStrLn "error!"
  (Fine r) <- runVKT (k 33)
  putStrLn (show r)

