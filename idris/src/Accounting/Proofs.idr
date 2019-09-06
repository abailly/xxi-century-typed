||| Proofs for Group Properties of Balance
||| Recall that a Group must satisfy the following laws:
||| * Associativity of `<+>`:
|||     ```forall a b c, a <+> (b <+> c) == (a <+> b) <+> c```
||| * Neutral for `<+>`:
|||     ```
|||     forall a,     a <+> neutral   == a
|||     forall a,     neutral <+> a   == a
|||     ```
||| * Inverse for `<+>`:
|||     ```
|||     forall a,     a <+> inverse a == neutral
|||     forall a,     inverse a <+> a == neutral
|||     ```
module Accounting.Proofs

import Accounting.Amount
import Accounting.Core

import Control.Algebra
import Data.Vect
import Date
import Decidable.Order
import public Decidable.Equality

%default total


-- Proofs for neutral are pretty trivial as they are directly reducible
-- from the definition of `<+>`
balanceRightNeutral : (b : Balance) -> (b <+> Zero = b)
balanceRightNeutral Zero      = Refl
balanceRightNeutral (Bal n d) = Refl

balanceLeftNeutral : (b : Balance) -> (Zero <+> b = b)
balanceLeftNeutral Zero      = Refl
balanceLeftNeutral (Bal n d) = Refl

-- Proof of (right) inverse is slightly more involved as we need to check the 2 cases
-- for `decEq n n` stemming from the definition of `<+>`.
-- We also need to expand the `Bal`'s amount to `MkAmount n` in order to trigger the
-- reducing of `compensate`. And for the same reason but applied to `inverse` we need
-- to pattern match on the direction too.
balanceRightInverse : (b : Balance) -> (b <+> inverse b = Zero)
balanceRightInverse Zero      = Refl
balanceRightInverse (Bal (MkAmount n) Dr) with (decEq n n)
  | (Yes prf)   = Refl
  | (No contra) = absurd (contra Refl)

balanceRightInverse (Bal (MkAmount n) Cr) with (decEq n n)
  | (Yes prf) = Refl
  | (No contra) = absurd (contra Refl)


-- Lemmas proved while trying to prove associativity
-- I gave up at some point as it became more and more tedious, with dozens of cases to unfold because
-- of the way `<+>` and `compensate` are defined.
-- I will need to think more about the types, maybe to embed more "proofs" in the types themselves to
-- limit the number of cases to reduce

||| Balances are equal when their `Amount` is made of equal `Nat`
||| This proof uses `lteUniqueProof` provided in `Amount` in order to ensure
||| the compiler has the needed implicits that n and m are greater than 0.
eqNatIsEqAmount : (n = m)
                -> {notZeron : LTE 1 n} -> {notZerom : LTE 1 m}
                -> (Bal (MkAmount n) d = Bal (MkAmount m) d)
eqNatIsEqAmount Refl {notZeron} {notZerom} =
  rewrite lteUniqueProof notZeron notZerom in Refl

||| Lift associativity of `Nat`s into `Balance`
||| This is trivial once we have `eqNatIsEqAmount` thanks to prelude's `plusAssociative`
amountAssociative : (notZeron : LTE 1 n) -> (notZerom : LTE 1 m) -> (notZerok : LTE 1 k)
                  -> { auto prf1 : LTE 1 (plus n (plus m k)) }
                  -> { auto prf2 : LTE 1 (plus (plus n m) k) }
                  -> (Bal (MkAmount (plus n (plus m k))) z = Bal (MkAmount (plus (plus n m) k)) z)
amountAssociative notZeron notZerom notZerok {n} {m} {k} =
  eqNatIsEqAmount (plusAssociative n m k)

||| A simpler version of `succInjective` from `Prelude.Nat`
succInj : (S n = S m) -> n = m
succInj Refl = Refl

||| Proof that adding a number greater than 0 to any number cannot yield 2 equal numbers
||| This looks obvious but was actually quite tricky to write. Pattern matching on the equality
||| does not work: The hole gets filled by an `impossible` statement but this is wrong. And
||| manually matching on `Refl` gives a type error
notPlusSucc : (n : Nat) -> Not (n + S m = n)
notPlusSucc Z     prf = SIsNotZ prf
notPlusSucc (S n) prf = notPlusSucc n (succInj prf)

||| A very specialised lemma to prove a `Balance` cannot be zero under some (obvious) conditions
||| The `Z` cases for each of `m` and `k` easily yield `impossible` statements.
zeroNotEqBal : (x : plus m k = m) -> (prf : plus m k = k) -> {auto notZerom : LTE 1 m} -> {notZerok : LTE 1 k} -> (Zero = Bal (MkAmount k) z)
zeroNotEqBal {m = Z}     {k}         _ _ impossible
zeroNotEqBal {m}         {k = Z}     x prf impossible
-- it's unclear to me why we need to match explicitly the k but this seems to trigger
-- the reduction of `notPlusSucc` correctly so that the types line up for `m + S k = m`.
-- I suppose the typechecker does not know at this stage the only possible value for
-- k is `S k` as the other cases are covered before?
zeroNotEqBal {m = m}     {k = (S k)} x prf = absurd (notPlusSucc m x)


||| (Partial) proof that <+> is associative
||| The cases involving `Zero`s are easy, things go awry in the general case.
balanceAssociative : (a,b,c : Balance) -> (a <+> (b <+> c) = (a <+> b) <+> c)
balanceAssociative Zero      Zero      c = Refl
balanceAssociative (Bal n d) Zero      c = Refl
balanceAssociative Zero      (Bal n d) c = Refl
balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) Zero with (decEq y d)
  | (Yes prf) = Refl
  | (No contra) with (decEq n m)
    | (Yes prf) = Refl
    | (No f) with (order {to=LTE} n m)
      | (Left l) = Refl
      | (Right r) = Refl

balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) with (decEq d z)
  balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes prf) with (decEq y d)
    balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes prf) | (Yes x) with (decEq y z)
      balanceAssociative (Bal (MkAmount n {notZero=notZeron}) z) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (Yes Refl) | (Yes Refl) =
        amountAssociative notZeron notZerom notZerok
      balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) (Bal (MkAmount k) z) | (Yes prf) | (Yes x) | (No contra) with (decEq (n + m) k)
        balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount (plus n m)) z) | (Yes Refl) | (Yes Refl) | (No contra) | (Yes Refl) = absurd (contra Refl)
        balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) (Bal (MkAmount k ) z) | (Yes prf) | (Yes x) | (No contra) | (No f) with (order {to=LTE} (n + m) k)
          balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount k) z) | (Yes Refl) | (Yes Refl) | (No contra) | (No f) | (Left l) = absurd (contra Refl)
          balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount k) z) | (Yes Refl) | (Yes Refl) | (No contra) | (No f) | (Right r) = absurd (contra Refl)

    balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) with (decEq n (m + k))
      balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z)     | (Yes Refl) | (No contra) | (Yes Refl) with (decEq (m + k) k)
        balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z)   | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) with (decEq (m + k) m)
          balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) | (Yes x) = zeroNotEqBal x prf
          balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) | (No f)  = believe_me "proof is way too involved which probably means there is something wrontg in the types"


        balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (No f) = believe_me "proof is way too involved which probably means there is something wrontg in the types"

      balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (No f) = believe_me "proof is way too involved which probably means there is something wrontg in the types"


  | (No contra) = believe_me "proof is way too involved which probably means there is something wrontg in the types"
