{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Data
import Data.Typeable
import Data.Generics
import Data.Maybe
import Data.Char

import Debug.Trace


data Foo x = Foo Bar Int x
  deriving(Show,Data,Typeable)

data Bar = Bar Char Bool
  deriving(Show,Data,Typeable)


-- gmap Maps over immediate subterms
t1 = gmapT (\x ->
    fromJust (cast (Foo (Bar '!' True) 0 "str"))
  )
  (Foo (Bar 'x' False) 33 "bar")


t2 = gmapT
     (\x ->
      trace (show (dataTypeOf x) ++ "\n") $
      x
        -- case cast x of
        --   Nothing -> x
        --   Just (x :: Foo String) ->
        --     trace ("(picked " ++ (show x) ++ ")") $ fromJust $ cast $ Foo 0 "!"
     )
     $ Just (Foo (Bar 'x' False) 33 "bar")

t3 = gmapT
     (\d ->
        case cast d of
          Nothing -> d
          Just x ->
            fromJust (cast (if isUpper (head x) then "!" else x))
     )
     $ (Foo (Bar 'x' False) 33 "Bar")

data L1 a = C0 | C1 a | OP2 (L1 a) (L1 a) | OP1 (L1 a) | OP3 (L1 a) (L1 a) (L1 a) | C2 a a
  deriving(Show, Data, Typeable)

type XL1 = L1 String
type XL2 = L1 Int

ex1 = OP3 (OP1 (C1 "33")) (C2 "44" "55") C0


-- convert :: (String -> Int) -> XL1 -> XL2
-- convert fn d = gfoldl

data ID x = ID { unID :: x }
  deriving(Show)

t4 :: XL1
t4 = unID $ gfoldl
      (\fdb d ->
        let
          msg :: (Data x) => x -> String
          msg x = ((show (toConstr x)) ++ " of " ++ (show (dataTypeOf x)))
        in
        trace (msg d) $
        ID $
        -- trace (" --> " ++ (msg $ (unID fdb) d)) $
        (unID fdb) d
      )
      (\x ->
        trace ("triv") $
        ID x
      )
      ex1



