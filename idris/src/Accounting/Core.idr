module Accounting.Core

import Accounting.Amount

import Control.Algebra
import Data.Vect
import Date
import Decidable.Order
import public Decidable.Equality

%default total

public export
data Direction : Type where
  Dr : Direction
  Cr : Direction

public export
Eq Direction where
  Dr == Dr = True
  Cr == Cr = True
  _  == _  = False

Show Direction where
  show Dr = "D"
  show Cr = "C"

public export
DecEq Direction where
  decEq Dr Dr = Yes Refl
  decEq Cr Cr = Yes Refl
  decEq Dr Cr = No $ \ Refl impossible
  decEq Cr Dr = No $ \ Refl impossible



||| A Balance is a debit or a credit of a certain `Amount` or `Zero`
public export
data Balance : Type where
  ||| A Balance can be `Zero` in which case we don't care whether or
  ||| not it's a Debit or Credit.
  ||| The "natural" 0 of an account will depend on its type, eg. an `Asset`
  ||| should be Debit hence its 0 should be a debit too
  Zero : Balance

  ||| If a `Balance` is not 0, then it's an `Amount` (a strictly positive
  ||| natural integer and a `Direction`
  Bal : (n : Amount) -> (d : Direction) -> Balance

%name Balance z, bal

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

compensate : (n : Amount) -> (d : Direction) -> (n' : Amount) -> (d' : Direction) -> Balance
compensate (MkAmount n) d (MkAmount n') d' with (decEq n n')
    | (Yes prf) = Zero
    | (No contra) with (order {to=LTE} n n')
      | (Left l)  = Bal (MkAmount (n' - n) { notZero = notEqualMinusGTOne n n' l contra }) d'
      | (Right r) = Bal (MkAmount (n - n') { notZero = notEqualMinusGTOne n' n r (notEqReflexive contra) }) d

public export
Semigroup Balance where
  Zero      <+> y           = y
  x         <+> Zero        = x
  (Bal n d) <+> (Bal n' d') with (decEq d d')
    | (Yes prf) = Bal (n + n') d
    | (No contra) = compensate n d n' d'

public export
Monoid Balance where
  neutral = Zero

public export
Group Balance where
  inverse Zero = Zero
  inverse (Bal n Dr) = Bal n Cr
  inverse (Bal n Cr) = Bal n Dr

-- I would like to prove Balance actually respects Semigroup/Monoid/Group properties but this will
-- be left for another session
-- Must satisfy the following laws:
-- + Associativity of `<+>`:
--     forall a b c, a <+> (b <+> c) == (a <+> b) <+> c
-- + Neutral for `<+>`:
--     forall a,     a <+> neutral   == a
--     forall a,     neutral <+> a   == a
-- + Inverse for `<+>`:
--     forall a,     a <+> inverse a == neutral
--     forall a,     inverse a <+> a == neutral

-- balanceRightNeutral : (b : Balance) -> (b <+> Zero = b)
-- balanceRightNeutral Zero      = Refl
-- balanceRightNeutral (Bal n d) = Refl

-- balanceLeftNeutral : (b : Balance) -> (Zero <+> b = b)
-- balanceLeftNeutral Zero      = Refl
-- balanceLeftNeutral (Bal n d) = Refl

balanceRightInverse : (b : Balance) -> (b <+> inverse b = Zero)
balanceRightInverse Zero      = Refl
balanceRightInverse (Bal n Dr) = ?balanceRightInverse_rhs_1
balanceRightInverse (Bal n Cr) = ?balanceRightInverse_rhs_3

-- balanceAssociative : (a,b,c : Balance) -> (a <+> (b <+> c) = (a <+> b) <+> c)
-- balanceAssociative Zero      Zero      c = Refl
-- balanceAssociative (Bal n d) Zero      c = Refl
-- balanceAssociative Zero      (Bal n d) c = Refl
-- balanceAssociative (Bal x y) (Bal n d) c with (decEq y d)
--   balanceAssociative (Bal x y) (Bal n d) c | (Yes prf) = ?op_rhs_1
--   balanceAssociative (Bal x y) (Bal n d) c | (No contra) = ?op_rhs_2


public export
data AccountType : Type  where
  Asset : AccountType
  Liability : AccountType
  Equity : AccountType
  Expense : AccountType
  Revenue : AccountType

public export
Eq AccountType where
  Asset     == Asset     = True
  Liability == Liability = True
  Equity    == Equity    = True
  Expense   == Expense   = True
  Revenue   == Revenue   = True
  _         == _         = False

public export
Show AccountType where
  show Asset = "Asset"
  show Liability = "Liability"
  show Equity = "Equity"
  show Expense = "Expense"
  show Revenue = "Revenue"

public export
data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

public export
Eq Account where
  (MkAccount lbl {type=t}) == (MkAccount lbl' {type=t'}) = lbl == lbl' && t == t'

public export
Show Account where
  show (MkAccount lbl {type=t}) = show t ++ ":" ++ lbl

public export
isA : AccountType -> Account -> Bool
isA t (MkAccount _ {type}) = t == type

public export
record Entry where
  constructor MkEntry
  amount : Amount
  direction : Direction
  account : Account

Eq Entry where
  (MkEntry amount dir acc) == (MkEntry amount' dir' acc') = amount == amount' && dir == dir' && acc == acc'

Show Entry where
  show (MkEntry amt dir acc) = "  " ++ show acc ++ " " ++ show dir ++ " " ++ show amt

public export
Cast Entry Balance where
  cast (MkEntry amount direction account) = Bal amount direction

||| Provide the balance for a list of entries
||| When there is not entry, by convention it returns `(0, Cr)`,
||| otherwise it returns the difference between debits `Dr` and credits
||| `Cr` from all transactions.
total
public export
balance : Vect n Entry -> Balance
balance =  concat . map cast

public export
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = Zero } -> Entries


public export
withSameLength : (en : Vect n' Entry) -> (prf : n = n') -> Vect n Entry
withSameLength en prf = rewrite prf in en

Eq Entries where
  (MkEntries en {n=n}) == (MkEntries en' {n=n'}) with (decEq n n')
     | (Yes prf) = withSameLength en' prf == en
     | (No _)    = False

Show Entries where
  show (MkEntries ens) = unlines (toList $ map show ens)

public export
record Transaction where
  constructor Tx
  label : String
  date : Date
  entries : Entries

Eq Transaction where
  (Tx lbl dat en) == (Tx lbl' dat' en') = lbl == lbl' && dat == dat' && en == en'

Show Transaction where
  show (Tx lbl dt entries) = show dt ++ " " ++ lbl ++ "\n" ++ show entries

public export
select : (Account -> Bool) -> Entries -> Balance
select selector (MkEntries entries) with (filter (selector . account) entries)
  | (_ ** filtered) = concat $ map cast filtered

public export
selectEntries : (Account -> Bool) -> Vect k Entries -> Balance
selectEntries selector entries = concat $ map (select selector) entries

public export
assets : Vect k Transaction -> Balance
assets = selectEntries (isA Asset) . map entries

public export
liabilities : Vect k Transaction -> Balance
liabilities = selectEntries (isA Liability) . map entries

public export
capital : Vect k Transaction -> Balance
capital = selectEntries (isA Equity) . map entries

public export
expenses : Vect k Transaction -> Balance
expenses = selectEntries (isA Expense) . map entries

public export
revenues : Vect k Transaction -> Balance
revenues = selectEntries (isA Revenue) . map entries

public export
data BookOfAccounts : Type where
  BookTransactions : (txs : Vect k Transaction) ->
                     { auto fundamentalEquation : inverse (assets txs <+> expenses txs) = liabilities txs <+> capital txs <+> revenues txs } ->
                     BookOfAccounts

export
Show BookOfAccounts where
  show (BookTransactions txs) = unlines (toList $ map show txs)


-- Testing

Capital : Account
Capital = MkAccount "Capital" {type = Equity}

Bank : Account
Bank = MkAccount "Bank" {type = Asset}

-- valid1 : balance [ MkEntry 100 Dr Bank,
--                    MkEntry 100 Cr Capital ] = Zero
-- valid1 = Refl

-- valid2 : balance [ MkEntry 100 Cr Bank,
--                   MkEntry 100 Dr Capital ] = Zero
-- valid2 = Refl

-- invalid : Not (balance [ MkEntry 100 Cr Bank,
--                          MkEntry 101 Dr Capital ] = Zero)
-- invalid = \ Refl impossible

-- tx : Transaction
-- tx = Tx "Some transaction" (MkDate 2019 January 01) $ MkEntries [ MkEntry 100 Dr Bank,
--                                                                   MkEntry 100 Cr Capital ]

-- book1 : BookOfAccounts
-- book1 = BookTransactions [ tx ]
