module Main where

import Control.Arrow((>>>))
import Data.Maybe
import Data.List(findIndex)

newtype Message = Msg { unMsg :: [Int] }
  deriving (Eq, Show)

type SubstitutionTable = Int -> Int

mkSubst :: [Int] -> SubstitutionTable
mkSubst table n =
  table !! (n - 1)

cipher :: SubstitutionTable -> Message -> Message
cipher table = Msg . fmap table . unMsg

powerOfSubst :: SubstitutionTable -> Message -> Message -> Int
powerOfSubst table initial target =
  let ciphers = drop 1 $ iterate (cipher table) initial
  in 1 + (fromJust $ findIndex (uncurry (==)) $ zip ciphers (repeat target))

doSolve :: [String] -> [String]
doSolve (_:t:c:code:rest) =
  let initial = Msg $ fmap read $ words t
      target = Msg $ fmap read $ words c
      subst = mkSubst $ fmap read $ words code
  in show (powerOfSubst subst initial target) : doSolve rest
doSolve _ = []

solve :: String -> String
solve =
  lines >>> drop 1 >>> doSolve >>> unlines

main :: IO ()
main =
  interact $ solve
