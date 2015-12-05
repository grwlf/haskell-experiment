{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cont where


import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans


data Result t a =
    Fine a
  | EInt Int (Int -> t (Result t a) a)
  | EBool Bool (Bool -> t (Result t a) a)


newtype VKT m r a = VKT { unVK :: ContT r m a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadIO)

liftCont :: ((a -> m r) -> m r) -> VKT m r a
liftCont f = VKT (ContT f)

-- class MonadVK m m' where
--   raiseError :: ((z -> VKT (Result VKT m x) m x) -> Result VKT m x) -> VKT (Result VKT m x) m z

raiseError :: (Monad m) => ((z -> VKT m (Result (VKT m) x) x) -> Result (VKT m) x) -> VKT m (Result (VKT m) x) z
raiseError ctr = liftCont (\cont -> do
  return (ctr

    (\x -> liftCont (\cont' -> do
      res <- cont x
      case res of
        Fine a -> cont' a
        x -> return x)
    )

    ))

