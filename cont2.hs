{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Cont where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.State

data Result t a =
    Fine a
  | EInt (Int -> t (Result t a) (Result t a))
  | EBool (Bool -> t (Result t a) (Result t a))

type Guts x m r a = StateT (r -> x m r r) (ContT r m) a

newtype VKT m r a = VKT { unVKT :: Guts VKT m r a }
  deriving(Functor, Applicative, Monad, MonadState (r -> VKT m r r),  MonadCont, MonadIO)

class (MonadCont m, MonadState (r -> m r) m) => MonadVK m r

instance MonadVK (VKT m r) r

runVKT :: (Monad m) => VKT m r r -> m r
runVKT m = runContT (evalStateT (unVKT (catch m)) undefined) return

catch :: (MonadVK m r) => m r -> m r
catch m = do
  callCC $ \k -> do
    modify (const k)
    m

raise :: (MonadVK m r) => ((a -> m b) -> r) -> m a
raise z = callCC $ \k -> do
  err <- get
  err (z k)
  undefined


-- test1 :: IO ()
-- test1 = do
--   (EInt k) <- runVKT $ do
--                   liftIO $ putStrLn "p1"
--                   x <- raise EInt
--                   liftIO $ putStrLn $ "x=" ++ (show x)
--                   return (Fine (42::Int))
--   putStrLn "error!"
--   (Fine r) <- runVKT (k 33)
--   putStrLn (show r)

