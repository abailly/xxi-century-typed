{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
module Main where

import Control.Monad(forM, forM_)
import Control.Monad.ST
import Data.Array.ST
import Control.Arrow((>>>))

mkSubst :: forall s . [Int] -> Int -> ST s (STUArray s Int Int -> ST s ())
mkSubst table len = do
  subst :: STUArray s Int Int <- newListArray (1,100 :: Int) table
  let apply arr = forM_ [ 1 .. len :: Int ] (\ i -> readArray arr i >>= readArray subst >>= writeArray arr i)
  pure apply

powerOfSubst :: forall s . Int -> [Int] -> [Int] -> [Int] -> ST s Int
powerOfSubst len table initial target = do
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


doSolve :: forall s . [String] -> ST s [Int]
doSolve (l:t:c:code:rest) = do
  let
    len = read l
    initial = fmap read $ words t
    target = fmap read $ words c
    subst = fmap read $ words code
  num <- powerOfSubst len subst initial target
  other <- doSolve rest
  pure $ num : other
doSolve _ = pure []

solve :: String -> String
solve =
  lines >>> drop 1 >>> (\ s -> runST (doSolve s)) >>> fmap show  >>> unlines

main :: IO ()
main =
  interact $ solve
