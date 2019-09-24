module Accounting.Core

import Accounting.Amount

import Control.Algebra
import Data.Vect
import Data.SortedMap as SMap
import Date
import Decidable.Order
import public Decidable.Equality

%access public export
%default total

||| Direction of a cash movement
|||
||| It's either a _Debit_ (`Dr`) or a _Credit_ (`Cr`).
||| * A _Debit_ is an `Amount` that _enters_ an account
||| * A _Credit_ is an `Amount` that _exits_ an account
data Direction : Type where
  ||| A Debit operation
  Dr : Direction

  ||| A Credit operation
  Cr : Direction

Eq Direction where
  Dr == Dr = True
  Cr == Cr = True
  _  == _  = False

Show Direction where
  show Dr = "D"
  show Cr = "C"

DecEq Direction where
  decEq Dr Dr = Yes Refl
  decEq Cr Cr = Yes Refl
  decEq Dr Cr = No $ \ Refl impossible
  decEq Cr Dr = No $ \ Refl impossible


||| A Balance is a debit or a credit of a certain `Amount` or `Zero`
data Balance : Type where
  ||| A Balance can be `Zero` in which case we don't care whether or
  ||| not it's a Debit or Credit.
  ||| The "natural" 0 of an account will depend on its type, eg. an `Asset`
  ||| should be Debit hence its 0 should be a debit too
  Zero : Balance

  ||| If a `Balance` is not 0, then it's an `Amount` (a strictly positive
  ||| natural integer and a `Direction`
  Bal : (n : Amount) -> (d : Direction) -> Balance

-- Lemmas used in the definition of `compensate`

notEqSuccIsNotEq : ((S left = S right) -> Void) -> (left = right) -> Void
notEqSuccIsNotEq f Refl = f Refl

lteAndNotEqIsLT : LTE n m -> ((n = m) -> Void) -> LTE (S n) m
lteAndNotEqIsLT LTEZero     f {m = Z}         = absurd (f Refl)
lteAndNotEqIsLT LTEZero     f {m = (S k)}     = LTESucc LTEZero
lteAndNotEqIsLT (LTESucc x) f {m = (S right)} = LTESucc (lteAndNotEqIsLT x (notEqSuccIsNotEq f))

ltMinusIsLTZero : LTE (S n) m -> LTE 1 (minus m n)
ltMinusIsLTZero (LTESucc LTEZero)     = LTESucc LTEZero
ltMinusIsLTZero (LTESucc (LTESucc x)) = ltMinusIsLTZero (LTESucc x)

notEqualMinusGTOne : (n, n' : Nat) -> (l : LTE n n') -> (contra : (n = n') -> Void) -> LTE 1 (n' - n)
notEqualMinusGTOne n n' l contra =
  let lt = lteAndNotEqIsLT l contra
  in ltMinusIsLTZero lt

notEqReflexive : ((m = n) -> Void) -> ((n = m) -> Void)
notEqReflexive f Refl = f Refl

||| The "sum" of 2 `Amount`s with a `Direction`
||| This is really just a signed addition in the group of Relative numbers (eg. `Z`) but
||| it is adapted to the structure of a `Balance`: We need to provide the proofs that
||| the resulting amount is greater than 0 which involves a few lemmas.
compensate : (n : Amount) -> (d : Direction) -> (n' : Amount) -> (d' : Direction) -> { auto notEqDir : Not (d = d') } -> Balance
compensate (MkAmount n) d (MkAmount n') d' with (decEq n n')
    | (Yes prf) = Zero
    | (No contra) with (order {to=LTE} n n')
      | (Left l)  = Bal (MkAmount (n' - n) { notZero = notEqualMinusGTOne n n' l contra }) d'
      | (Right r) = Bal (MkAmount (n - n') { notZero = notEqualMinusGTOne n' n r (notEqReflexive contra) }) d

Semigroup Balance where
  Zero      <+> y           = y
  x         <+> Zero        = x
  (Bal n d) <+> (Bal n' d') with (decEq d d')
    | (Yes prf) = Bal (n + n') d
    | (No contra) = compensate n d n' d' { notEqDir = contra }

Monoid Balance where
  neutral = Zero

Group Balance where
  inverse Zero = Zero
  inverse (Bal n Dr) = Bal n Cr
  inverse (Bal n Cr) = Bal n Dr

private
directionInj : (Bal n d = Bal x y) -> d = y
directionInj Refl = Refl

private
amountInj : (Bal n d = Bal m d') -> (n = m)
amountInj Refl = Refl

DecEq Balance where
  decEq Zero Zero = Yes Refl
  decEq Zero (Bal n d) = No $ \ Refl impossible
  decEq (Bal n d) Zero = No $ \ Refl impossible
  decEq (Bal n d) (Bal x y) with (decEq n x, decEq d y)
    decEq (Bal n y) (Bal n y) | (Yes Refl, Yes Refl) = Yes Refl
    decEq (Bal n d) (Bal x y) | (No contra, b) = No (contra . amountInj)
    decEq (Bal n d) (Bal x y) | (a, No contra) = No (contra . directionInj)

Show Balance where
  show Zero = "0"
  show (Bal n d) = show d ++ " " ++ show n

||| The 5 basic categories of accounts
data AccountType : Type  where
  Asset : AccountType
  Liability : AccountType
  Equity : AccountType
  Expense : AccountType
  Revenue : AccountType

Eq AccountType where
  Asset     == Asset     = True
  Liability == Liability = True
  Equity    == Equity    = True
  Expense   == Expense   = True
  Revenue   == Revenue   = True
  _         == _         = False

Show AccountType where
  show Asset = "Asset"
  show Liability = "Liability"
  show Equity = "Equity"
  show Expense = "Expense"
  show Revenue = "Revenue"

||| An `Account`
||| I initially tried to make the `AccountType` part of the type of the `Account`
||| but this caused to be problematic when trying to categorize the accounts. I
||| should probably try again as it seems to make more sense.
data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

Eq Account where
  (MkAccount lbl {type=t}) == (MkAccount lbl' {type=t'}) = lbl == lbl' && t == t'

Ord Account where
  compare (MkAccount x {type}) (MkAccount x' {type=type'}) =
    compare x x'

Show Account where
  show (MkAccount lbl {type=t}) = show t ++ ":" ++ lbl

||| Classify an `Account` according to its `AccountType`.
isA : AccountType -> Account -> Bool
isA t (MkAccount _ {type}) = t == type

||| A single `Entry` in a ledger.
||| An `Entry` is never used alone, it's used as a constituent of a `Transaction` and can
||| later on be displayed part of one or several `Account`s register.
||| An `Entry` can never be `Zero` hence we do not use a `Balance` to store the `Entry`s
||| amount but provide a `Cast Entry Balance` implementation so that  we can interpret
||| an `Entry` as a `Balance` should we need to.
record Entry where
  constructor MkEntry
  amount : Amount
  direction : Direction
  account : Account

Eq Entry where
  (MkEntry amount dir acc) == (MkEntry amount' dir' acc') = amount == amount' && dir == dir' && acc == acc'

Show Entry where
  show (MkEntry amt dir acc) = "  " ++ show acc ++ " " ++ show dir ++ " " ++ show amt

Cast Entry Balance where
  cast (MkEntry amount direction account) = Bal amount direction

||| Provides the balance for a list of entries
||| This boils down to `cast` the entries to `Balance`s and use the `Monoid` property of
||| Balance to sum them all.
total
balance : Vect n Entry -> Balance
balance =  concat . map cast


||| A wrapper over a sequence of `Entry`s
||| This type guarantees that the sequence of `Entry`s provided:
||| * Has at least 2 elements (otherwise, it could not be balanced),
||| * Is propertly _balanced_
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = Zero } -> Entries


withSameLength : (en : Vect n' Entry) -> (prf : n = n') -> Vect n Entry
withSameLength en prf = rewrite prf in en

Eq Entries where
  (MkEntries en {n=n}) == (MkEntries en' {n=n'}) with (decEq n n')
     | (Yes prf) = withSameLength en' prf == en
     | (No _)    = False

Show Entries where
  show (MkEntries ens) = unlines (toList $ map show ens)

||| A transaction is simply a balanced sequence of entries with a label and a date
record Transaction where
  constructor Tx
  label : String
  date : Date
  entries : Entries

Eq Transaction where
  (Tx lbl dat en) == (Tx lbl' dat' en') = lbl == lbl' && dat == dat' && en == en'

Show Transaction where
  show (Tx lbl dt entries) = show dt ++ " " ++ lbl ++ "\n" ++ show entries

-- Simple values for testing

Capital : Account
Capital = MkAccount "Capital" {type = Equity}

Bank : Account
Bank = MkAccount "Bank" {type = Asset}

namespace CoreTest
  %access private
  valid1 : balance [ MkEntry 100 Dr Bank
                   , MkEntry 100 Cr Capital ] = Zero
  valid1 = Refl

  valid2 : balance [ MkEntry 100 Cr Bank
                   , MkEntry 100 Dr Capital ] = Zero
  valid2 = Refl

  invalid : Not (balance [ MkEntry 100 Cr Bank
                         , MkEntry 101 Dr Capital ] = Zero)
  invalid = \ Refl impossible

  tx : Transaction
  tx = Tx "Some transaction" (MkDate 2019 January 01) $ MkEntries [ MkEntry 100 Dr Bank
                                                                  , MkEntry 100 Cr Capital ]
