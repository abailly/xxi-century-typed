module BottlesOfBeer

import Prelude.Nat as Nat
import Decidable.Order  as Order

%default total

data Bottles : (n : Nat) -> Type where
  BottlesNoMore :                           Bottles 0
  OneBottle     :                           Bottles 1
  TwoBottles    :                           Bottles 2
  MoreBottles   : (n : Nat)
                -> { auto p : LTE n 96 } -> Bottles (S (S (S n)))


verse : Bottles n -> String
verse BottlesNoMore =
  """No more bottles of beer on the wall,
no more bottles of beer.
Go to the store and buy some more,
99 bottles of beer on the wall.\n"""
verse OneBottle =  """1 bottle of beer on the wall,
1 bottle of beer.
Take it down and pass it around,
no more bottles of beer on the wall.\n"""
verse TwoBottles = """2 bottles of beer on the wall,
2 bottles of beer.
Take one down and pass it around,
1 bottle of beer on the wall.\n"""
verse (MoreBottles n ) =
  unlines [ show (n + 3) ++ " bottles of beer on the wall,"
          , show (n + 3) ++ " bottles of beer.\n"
          , "Take one down and pass it around,"
          , (show $ n + 2) ++ " bottles of beer on the wall.\n"
          ]

drinkOne : Bottles (S n) -> (String, Bottles n)
drinkOne b@OneBottle               = (verse b, BottlesNoMore)
drinkOne b@TwoBottles              = (verse b, OneBottle)
drinkOne b@(MoreBottles Z )        = (verse b, TwoBottles)
drinkOne b@(MoreBottles (S k) {p}) = (verse b, MoreBottles k { p = lteSuccLeft p })

verses : Bottles (S n) -> List String
verses {n = Z}     OneBottle
                        = [verse OneBottle, verse BottlesNoMore ]
verses {n = (S k)} x with (drinkOne x)
   | (v, b)             = v :: verses {n= k} b

song : String
song = concat $ verses (MoreBottles 96)
