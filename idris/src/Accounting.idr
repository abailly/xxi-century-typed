||| # Depdendently-Typed Accounting
|||
||| The goal of this module is to demonstrate how dependent can be useful
||| in defining invariants for business domains.
|||
||| ## Accounting Rules
|||
||| ### Reference
|||
||| From https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system
|||
||| > In the double-entry accounting system, at least two accounting
||| > entries are required to record each financial transaction. These
||| > entries may occur in asset, liability, equity, expense, or revenue
||| > accounts. Recording of a debit amount to one or more accounts and
||| > an equal credit amount to one or more accounts results in total
||| > debits being equal to total credits for all accounts in the general
||| > ledger. If the accounting entries are recorded without error, the
||| > aggregate balance of all accounts having Debit balances will be
||| > equal to the aggregate balance of all accounts having Credit
||| > balances. Accounting entries that debit and credit related accounts
||| > typically include the same date and identifying code in both
||| > accounts, so that in case of error, each debit and credit can be
||| > traced back to a journal and transaction source document, thus
||| > preserving an audit trail. The accounting entries are recorded in
||| > the "Books of Accounts". Regardless of which accounts and how many
||| > are impacted by a given transaction, the fundamental accounting
||| > equation of assets equal liabilities plus capital will hold.
|||
||| ### Actual Rules
|||
||| Actually, this fundamental equation is only true at the end of a period
||| because everyday transactions usually involve a revenue/expense account
||| and one or more assets/liabilities accounts. For example, making a sale
||| is recorded as (using this module's code):
|||
||| ```
||| Sale = Account "Sales" {type=Revenue}
||| Customer = Account "Customer" {type=Asset}
|||
||| Tx "Sell 1 Brouzouf" "1/1/2019" $ MkEntries [ MkEntry (100, Dr) Customer,
|||                                               MkEntry (100, Cr) Sale ]
||| ```
||| eg. we increase our revenue by 100 and we record the fact the customer has
||| a debt of 100.
||| Then when the customer pays, we get a bank transaction that balances the
||| customer's account
||| ```
||| Bank = Account "Bank" {type=Asset}
|||
||| Tx "Wire transfer" "2/1/2019" $ MkEntries [ MkEntry (100, Cr) Customer,
|||                                             MkEntry (100, Dr) Bank ]
||| ```
module Accounting

import Data.Vect
import Decidable.Order


public export
data Direction : Type where
  Dr : Direction
  Cr : Direction

public export
Eq Direction where
  Dr == Dr = True
  Cr == Cr = True
  _  == _  = False

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

Eq AccountType where
  Asset     == Asset     = True
  Liability == Liability = True
  Equity    == Equity    = True
  Expense   == Expense   = True
  Revenue   == Revenue   = True
  _         == _         = False

public export
data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

isA : AccountType -> Account -> Bool
isA t (MkAccount _ {type}) = t == type

export
Capital : Account
Capital = MkAccount "Capital" {type = Equity}

export
Bank : Account
Bank = MkAccount "Bank" {type = Asset}

public export
record Entry where
  constructor MkEntry
  amount : Balance
  account : Account

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

valid1 : balance [ MkEntry (100, Dr) Bank,
                  MkEntry (100, Cr) Capital ] = (0, Cr)
valid1 = Refl

valid2 : balance [ MkEntry (100, Cr) Bank,
                  MkEntry (100, Dr) Capital ] = (0, Cr)
valid2 = Refl

invalid : Not (balance [ MkEntry (100, Cr) Bank,
                         MkEntry (101, Dr) Capital ] = (0, Cr))
invalid = \ Refl impossible

export
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = (0, Cr) } -> Entries

export
record Transaction where
  constructor Tx
  label : String
  date : String
  entries : Entries

select : (Account -> Bool) -> Entries -> (Nat, Direction)
select selector (MkEntries entries) with (filter (selector . account) entries)
  | (_ ** filtered) = concat $ map amount filtered

selectEntries : (Account -> Bool) -> Vect k Entries -> (Nat, Direction)
selectEntries selector entries = concat $ map (select selector) entries

assets : Vect k Transaction -> Balance
assets = selectEntries (isA Asset) . map entries

liabilities : Vect k Transaction -> Balance
liabilities = selectEntries (isA Liability) . map entries

capital : Vect k Transaction -> Balance
capital = selectEntries (isA Equity) . map entries

export
data BookOfAccounts : Type where
  BookTransactions : (txs : Vect k Transaction) ->
                     { auto fundamentalEquation : invert (assets txs) = liabilities txs <+> capital txs } ->
                     BookOfAccounts

book1 : BookOfAccounts
book1 = BookTransactions [ Tx "foo" "1/1/2019" $ MkEntries [ MkEntry (100, Dr) Bank,
                                                             MkEntry (100, Cr) Capital ]
                         ]
