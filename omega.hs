{-# LANGUAGE RankNTypes #-}
module Main where


om :: (x -> x) -> (x -> x)
om x = x x



-- f = (+5)
