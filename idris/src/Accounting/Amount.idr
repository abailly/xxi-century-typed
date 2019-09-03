module Accounting.Amount

import Decidable.Order
import public Decidable.Equality

%default total


-- Auxiliary lemmas for proving properties of Amount

||| A proof that the sum of 2 numbers greater than 1 is greater than 1
lteOnePlus : (nz : LTE 1 n) -> (nz' : LTE 1 k) -> LTE 1 (n + k)
lteOnePlus nz nz' {n} = lteTransitive nz (lteAddRight n)

||| Adding a `Nat` number to the right of an inequation preserves the ordering
plusRightIsLte : LTE j k -> LTE j (k + n)
plusRightIsLte x   {k} = lteTransitive x (lteAddRight k)

||| A proof that multiplying a number by any positive number yields a
||| greater or equal number.
lteMultRight : (n : Nat) -> { auto nz : LTE 1 k } -> LTE n (mult n k)
lteMultRight Z               = LTEZero
lteMultRight n     {k = Z}   impossible
lteMultRight (S j) {k = S k} =
  rewrite plusCommutative k (j * S k)
  in LTESucc (plusRightIsLte $ lteMultRight j {k = S k})


||| A proof that the product of 2 numbers greater than 1 is greater than 1
lteOneMult : (nz : LTE 1 n) -> (nz' : LTE 1 k) -> LTE 1 (n * k)
lteOneMult nz nz' {n} = lteTransitive nz (lteMultRight n)

||| An `Amount` is a strictly positive value
public export
data Amount : Type where
  MkAmount : (n : Nat) -> { auto notZero : LTE 1 n } -> Amount

||| Convert an `Integer` to an `Amount
||| Negative or null values are mapped to 1
fromIntegerAmount : Integer -> Amount
fromIntegerAmount x =
    let n = fromInteger x
    in case isLTE 1 n of
      (Yes prf)   => MkAmount n
      (No contra) => MkAmount (S Z)

||| Implementation of numeric operations over an Amount
||| This is basically lifting the operations from `Nat` but we need to
||| build a proof the result is greater than 0, which makes things  trickier
public export
Num Amount where
  (MkAmount n {notZero=nz}) + (MkAmount k {notZero=nz'}) = MkAmount (n + k) { notZero = lteOnePlus nz nz' }

  (MkAmount n {notZero=nz}) * (MkAmount k {notZero=nz'}) = MkAmount (n * k) { notZero = lteOneMult nz nz' }

  fromInteger = fromIntegerAmount