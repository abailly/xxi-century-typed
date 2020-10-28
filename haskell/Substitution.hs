{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
module Main where

import Control.Monad(forM, forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array as A
import Data.List(sort, nub)
import Control.Arrow((>>>))
import Crt

-- what's the number of iterations for given pair of numbers?
distance :: Array Int Int -> Int -> Int -> Int
distance table i k = go 1 i
  where
    go n j =
      let x = table ! j
      in if x == k
         then n
         else go (n+1)  x

-- What's the length of cycle for given number ?
cycleLength :: Array Int Int -> Int -> Int
cycleLength table i = go 1 i
  where
    go n j =
      let x = table ! j
      in if x == i
         then n
         else go (n+1)  x

mkSubst :: forall s . [Int] -> Int -> ST s (STUArray s Int Int -> ST s ())
mkSubst table len = do
  subst :: STUArray s Int Int <- newListArray (1,100 :: Int) table
  let apply arr = forM_ [ 1 .. len :: Int ] (\ i -> readArray arr i >>= readArray subst >>= writeArray arr i)
  pure apply

powerOfSubstST :: forall s . Int -> [Int] -> [Int] -> [Int] -> ST s Int
powerOfSubstST len table initial target = do
  f <- mkSubst table len
  start :: STUArray s Int Int <- newListArray (1,len :: Int) initial
  end  :: STUArray s Int Int <- newListArray (1,len :: Int) target
  go 0 f start end
    where
      go n f start end = do
        isEq <- and <$> forM [ 1..len] (\ i -> do
          x <- readArray start i
          y <- readArray end i
          pure $ x == y)
        if isEq
          then pure n
          else f start >> go (n+1) f start end

subst1 :: [Int]
subst1 = 100 : [ 1 .. 99 ]

powerOfSubst :: Int -> [Int] -> [Int] -> [Int] -> Int
powerOfSubst _len table initial target =
  let tbl = listArray (1,100) table
      dist = fmap (fromIntegral . uncurry (distance tbl)) $ zip initial target
      cycles = fmap (fromIntegral . cycleLength tbl) initial
  in fromInteger $ crt (nub $ sort $ zip cycles dist)

doSolve :: [String] -> [Int]
doSolve (l:t:c:code:rest) =
  let
    len = read l
    initial = fmap read $ words t
    target = fmap read $ words c
    subst = fmap read $ words code
  in powerOfSubst len subst initial target : doSolve rest
doSolve _ = []

doSolveST :: forall s . [String] -> ST s [Int]
doSolveST (l:t:c:code:rest) = do
  let
    len = read l
    initial = fmap read $ words t
    target = fmap read $ words c
    subst = fmap read $ words code
  s <- powerOfSubstST len subst initial target
  ss <- doSolveST rest
  pure $ s : ss
doSolveST _ = pure []

solve :: String -> String
solve =
  lines >>> drop 1 >>> doSolve >>> fmap show  >>> unlines

main :: IO ()
main = interact solve
