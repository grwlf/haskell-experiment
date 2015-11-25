{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import System.TimeIt

r :: [a] -> [a]
r [] = []
r (x:xs) = (r xs) ++ [x]

r' :: [a] -> [a] -> [a]
r' [] a = a
r' (x:xs) a = r' xs (x : a)

main = do

  forM_ [10..23] $ \n -> do
    putStrLn $ "l = " ++ (show (2^n))
    timeIt $ do

      !l <- return (r' [1..(2^n)] [])

      -- !l <- return (r [1..(2^n)])
      x <- return (length l)
      -- print x
      return x
      --
    return ()

  return ()





