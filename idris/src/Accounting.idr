module Accounting

import Data.Vect
import Decidable.Order

-- From https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system
-- In the double-entry accounting system, at least two accounting
-- entries are required to record each financial transaction. These
-- entries may occur in asset, liability, equity, expense, or revenue
-- accounts. Recording of a debit amount to one or more accounts and
-- an equal credit amount to one or more accounts results in total
-- debits being equal to total credits for all accounts in the general
-- ledger. If the accounting entries are recorded without error, the
-- aggregate balance of all accounts having Debit balances will be
-- equal to the aggregate balance of all accounts having Credit
-- balances. Accounting entries that debit and credit related accounts
-- typically include the same date and identifying code in both
-- accounts, so that in case of error, each debit and credit can be
-- traced back to a journal and transaction source document, thus
-- preserving an audit trail. The accounting entries are recorded in
-- the "Books of Accounts". Regardless of which accounts and how many
-- are impacted by a given transaction, the fundamental accounting
-- equation of assets equal liabilities plus capital will hold.

export
data Direction : Type where
  Dr : Direction
  Cr : Direction

Eq Direction where
  Dr == Dr = True
  Cr == Cr = True
  _  == _  = False


public export
data AccountType : Type  where
  Asset : AccountType
  Liability : AccountType
  Equity : AccountType
  Expense : AccountType
  Revenue : AccountType

export
data Account : (accType : AccountType) -> Type where
  MkAccount : String -> Account accType

export
capital : Account Equity
capital = MkAccount "capital"

export
bank : Account Asset
bank = MkAccount "bank"

record Entry where
  constructor MkEntry
  amount : Nat
  direction : Direction
  account : Account accType

||| Provide the balance for a list of entries
||| When there is not entry, by convention it returns `(0, Cr)`,
||| otherwise it returns the difference between debits `Dr` and credits
||| `Cr` from all transactions.
total
balance : Vect n Entry -> (Nat, Direction)
balance entries = balance' entries (Z, Cr)
  where
    balance' : Vect n Entry -> (Nat, Direction) -> (Nat, Direction)
    balance' [] (Z, Dr) = (Z, Cr)  -- this normalizes 0 to be a Credit operation
    balance' [] acc     = acc
    balance' ((MkEntry amount direction account) :: xs) (n, dir) =
      if direction == dir
      then balance' xs (amount + n, dir)
      else case order {to=LTE} n amount of
             (Left ltenamount)  => balance' xs (amount - n, direction)
             (Right lteamountn) => balance' xs (n - amount, dir)

valid : balance [ MkEntry 100 Cr Accounting.bank,
                  MkEntry 100 Dr Accounting.capital ] = (0, Cr)
valid = Refl

invalid : balance [ MkEntry 100 Cr Accounting.bank,
                    MkEntry 101 Dr Accounting.capital ] = (0, Cr)
invalid = ?hole

data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = (0, Cr) } -> Entries


record Transaction where
  constructor Tx
  label : String
  date : String
  entries : Entries
