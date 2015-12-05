{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cont where


import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans


data Result t m a =
    Fine a
  | MFine (m a)
  | EInt Int (Int -> t (Result t m a) m a)
  | EBool Bool (Bool -> t (Result t m a) m a)


newtype VKT r m a = VKT { unVK :: ContT r m a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadIO)

type Result2 m a = Result VKT m a

liftCont :: ((a -> m r) -> m r) -> VKT r m a
liftCont f = VKT (ContT f)

-- class MonadVK m m' where
--   raiseError :: ((z -> VKT (Result VKT m x) m x) -> Result VKT m x) -> VKT (Result VKT m x) m z

raiseError :: (Monad m) => ((z -> VKT (Result VKT m x) m x) -> Result VKT m x) -> VKT (Result VKT m x) m z
raiseError ctr = liftCont (\cont -> do
  return (ctr

    (\x -> liftCont (\cont' -> do
      res <- cont x
      case res of
        Fine a -> cont' a
        x -> return x)
    )

    ))

