module Accounting.Amount

import Decidable.Order
import public Decidable.Equality

%default total


-- Auxiliary lemmas for proving properties of Amount

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

public export
Eq Amount where
  (MkAmount n) == (MkAmount n') = n == n'

private
lteUniqueProof : (p, q : LTE a b) -> p = q
lteUniqueProof LTEZero LTEZero = Refl
lteUniqueProof (LTESucc x) (LTESucc y) =
  rewrite lteUniqueProof x y in
  Refl

private
eqNIsEqAmount : (prf : n = k)
      -> { notZeron : LTE 1 n }
      -> { notZerok : LTE 1 k }
      -> (MkAmount n = MkAmount k)
eqNIsEqAmount Refl {notZeron} {notZerok} =
  rewrite lteUniqueProof notZeron notZerok in Refl

public export
nInj : (MkAmount n {notZero} = MkAmount m {notZero=notZerom}) -> (n = m)
nInj Refl = Refl

public export
DecEq Amount where
  decEq (MkAmount n {notZero=notZeron}) (MkAmount k {notZero=notZerok}) with (decEq n k)
    | (Yes prf)   = Yes (eqNIsEqAmount prf)
    | (No contra) = No (contra . nInj)

public export
Show Amount where
  show (MkAmount n) = show n

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
  (MkAmount n {notZero=nz}) + (MkAmount k {notZero=nz'}) = MkAmount (n + k) { notZero = plusRightIsLte nz }

  (MkAmount n {notZero=nz}) * (MkAmount k {notZero=nz'}) = MkAmount (n * k) { notZero = lteOneMult nz nz' }

  fromInteger = fromIntegerAmount
