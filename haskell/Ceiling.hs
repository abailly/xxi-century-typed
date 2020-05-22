{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.List(group, sort)

data Tree a = E | N a (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Functor)

insert :: (Ord a) => [a] -> Tree a
insert list = insert' list E
  where
    insert' [] t = t
    insert' (a:as) E = insert' as (N a E E)
    insert' (a:as) (N x l r)
      | a < x = insert' as (N x (insert' [a] l) r)
      | otherwise = insert' as (N x l (insert' [a] r))

sample1 =
  unlines $ [ "5 3"
            , "2 7 1"
            , "3 1 4"
            , "1 5 9"
            , "2 6 5"
            , "9 7 3"
            ]

sample2 =
  unlines $ [ "3 4"
            , "3 1 2 40000"
            , "3 4 2 1"
            , "33 42 17 23"
            ]

mkTrees :: String -> [ Tree Int ]
mkTrees input =
  let lns = lines input
      listOfInt = fmap read . words
      [size, _] = listOfInt $ head lns
      layers = fmap listOfInt $ take size $ drop 1 $ lns
  in fmap insert layers

countShapes :: [ Tree a ] -> Int
countShapes =
  length . group . sort . fmap (fmap (const ()))

main :: IO ()
main = do
  input <- getContents
  print $ countShapes $ mkTrees input
