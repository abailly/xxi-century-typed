{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
module Main where

import Data.Array as A
import Data.List(findIndex)
import Data.Maybe
import Control.Arrow((>>>))

-- what's the number of iterations for given pair of numbers?
distance :: Array Int Int -> Int -> Int -> Int
distance table i k = go 1 i
  where
    go n j =
      let x = table ! j
      in if x == k
         then n
         else go (n+1)  x

subst1 :: [Int]
subst1 = 100 : [ 1 .. 99 ]

distances :: [Int] -> [Int] -> [Int] -> [Int]
distances table initial target =
  let tbl = listArray (1,100) table
  in fmap (uncurry (distance tbl)) $ zip initial target

powerOfSubst :: Int -> [Int] -> [Int] -> [Int] -> Int
powerOfSubst _len table initial target =
  let dist = distances table initial target
  in foldl1 lcm dist

doSolve :: [String] -> [Int]
doSolve (l:t:c:code:rest) =
  let
    len = read l
    initial = fmap read $ words t
    target = fmap read $ words c
    subst = fmap read $ words code
  in powerOfSubst len subst initial target : doSolve rest
doSolve _ = []

solve :: String -> String
solve =
  lines >>> drop 1 >>> doSolve >>> fmap show  >>> unlines

main :: IO ()
main = interact solve
