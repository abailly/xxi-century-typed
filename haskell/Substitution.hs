module Main where

import Data.Array
import Control.Arrow((>>>))
import Data.List (findIndex)
import Data.Maybe

newtype Message = Msg { unMsg :: Array Int Int }
  deriving (Eq, Show)

type SubstitutionTable = Int -> Int

mkSubst :: [Int] -> SubstitutionTable
mkSubst table n =
  let arr = array (1,100) (zip [1..] table)
  in arr ! n

subst1 = mkSubst $ [ 2, 3, 1, 5, 4 ] <> [ 6 .. 100 ]

cipher :: SubstitutionTable -> Message -> Message
cipher table = Msg . fmap table . unMsg

powerOfSubst :: SubstitutionTable -> Message -> Message -> Int
powerOfSubst table initial target =
  let ciphers = drop 1 $ iterate (cipher table) initial
  in 1 + (fromJust $ findIndex (uncurry (==)) $ zip ciphers (repeat target))

doSolve :: [String] -> [Int]
doSolve (l:t:c:code:rest) =
  let
      initial = Msg $ array (1,read l) $ zip [1..] $ fmap read $ words t
      target = Msg $ array (1,read l) $ zip [1..] $ fmap read $ words c
      subst = mkSubst $ fmap read $ words code
  in powerOfSubst subst initial target : doSolve rest
doSolve _ = []

solve :: String -> String
solve =
  lines >>> drop 1 >>> doSolve >>> fmap show >>> unlines

main :: IO ()
main =
  interact $ solve
