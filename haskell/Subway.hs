module Subway where

import Control.Arrow((>>>))

import Data.List(sort)

data Subway = Sub [Subway]
  deriving (Show, Eq, Ord)

fromString' :: [Subway] -> String -> Subway
fromString' [cur] [] = cur
fromString' (Sub ys : stack) ('0' : xs) =
  fromString' (Sub [] : (Sub (Sub [] : ys)) : stack) xs
fromString' (sub : (Sub (_ : ys)) : stack) ('1' : xs) =
  fromString' (Sub (sub : ys) : stack) xs
fromString' _ s = error $ "invalid Subway traversal: " <> s

fromString :: String -> Subway
fromString = fromString' [Sub []]

canonical :: Subway -> Subway
canonical (Sub ss) = Sub $ sort (map canonical ss)

groupBy2 :: [a] -> [(a, a)]
groupBy2 (x : y : rest) = (x,y) : groupBy2 rest

solve :: String -> String -> [String] -> [String]
solve a b acc =
  let sa = canonical $ fromString a
      sb = canonical $ fromString b
  in if sa == sb
     then "same" : acc
     else "different" : acc

subway :: String -> String
subway =
  lines >>> drop 1 >>> foldr (uncurry solve) [] . groupBy2 >>> unlines

main :: IO ()
main = interact subway
