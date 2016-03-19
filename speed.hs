{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- import Data.Array
import Data.Array.IO
import Data.Array.Base
import Data.Array.IO.Internals
-- import Data.Array.Unsafe
import Data.Foldable
import Data.IORef
import Control.Monad
import System.IO
import System.IO.Unsafe
-- import GHC.Int

loop :: Int -> (Int -> IO ()) -> IO ()
loop n act
  | n > 0 = do
    act n
    loop (n-1) act
  | otherwise = do
    return ()

-- a : [IORef Int]
n = 1000000

main :: IO ()
main = do
  !(ar :: IOUArray Int Int) <- newArray (0,n) (0 :: Int)
  loop 1000 $ \i -> do
    loop n $ \j -> do
      !v <- unsafeRead ar j
      unsafeWrite ar j (v+1)

  return ()

