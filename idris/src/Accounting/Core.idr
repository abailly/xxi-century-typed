module Accounting.Core

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

public export
Balance : Type
Balance = (Nat, Direction)

public export
Semigroup Balance where
  (n,d) <+> (n', d') =
        if d == d'
        then (n + n', d)
        else case order {to=LTE} n n' of
             (Left ltenn')  => (n' - n, d')
             (Right lten'n) => (n - n', d)

public export
Monoid Balance where
  neutral = (0, Cr)

||| Invert the direction of a `Balance`
public export
invert : Balance -> Balance
invert (bal, Cr) = (bal, Dr)
invert (bal, Dr) = (bal, Cr)

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
  amount : Balance
  account : Account

Eq Entry where
  (MkEntry amount acc) == (MkEntry amount' acc') = amount == amount' && acc == acc'

Show Entry where
  show (MkEntry (bal, dir) acc) = "  " ++ show acc ++ " " ++ show dir ++ " " ++ show bal

||| Provide the balance for a list of entries
||| When there is not entry, by convention it returns `(0, Cr)`,
||| otherwise it returns the difference between debits `Dr` and credits
||| `Cr` from all transactions.
total
public export
balance : Vect n Entry -> Balance
balance =  normalise . concat . map amount
  where
    normalise : Balance -> Balance
    normalise (Z, Dr) = neutral
    normalise bal     = bal

public export
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = (0, Cr) } -> Entries


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
select : (Account -> Bool) -> Entries -> (Nat, Direction)
select selector (MkEntries entries) with (filter (selector . account) entries)
  | (_ ** filtered) = concat $ map amount filtered

public export
selectEntries : (Account -> Bool) -> Vect k Entries -> (Nat, Direction)
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
                     { auto fundamentalEquation : invert (assets txs <+> expenses txs) = liabilities txs <+> capital txs <+> revenues txs } ->
                     BookOfAccounts

export
Show BookOfAccounts where
  show (BookTransactions txs) = unlines (toList $ map show txs)


-- Testing

Capital : Account
Capital = MkAccount "Capital" {type = Equity}

Bank : Account
Bank = MkAccount "Bank" {type = Asset}

valid1 : balance [ MkEntry (100, Dr) Bank,
                  MkEntry (100, Cr) Capital ] = (0, Cr)
valid1 = Refl

valid2 : balance [ MkEntry (100, Cr) Bank,
                  MkEntry (100, Dr) Capital ] = (0, Cr)
valid2 = Refl

invalid : Not (balance [ MkEntry (100, Cr) Bank,
                         MkEntry (101, Dr) Capital ] = (0, Cr))
invalid = \ Refl impossible

tx : Transaction
tx = Tx "Some transaction" (MkDate 2019 January 01) $ MkEntries [ MkEntry (100, Dr) Bank,
                                                      MkEntry (100, Cr) Capital ]

book1 : BookOfAccounts
book1 = BookTransactions [ tx ]
