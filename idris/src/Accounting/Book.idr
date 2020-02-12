module Accounting.Book

import Accounting.Amount
import Accounting.Core
import Date

import Control.Algebra
import Data.SortedMap as SMap
import Data.Vect

||| Compute the balance of filtered `Entries`
||| I use a `with ...` clause here in order to be able to use `select` in a
||| type-level expression and produce a sequence of entries with a known
||| length.
select : (Account -> Bool) -> Entries -> Balance
select selector (MkEntries entries) with (filter (selector . account) entries)
  | (_ ** filtered) = concat $ map cast filtered

||| Aggregated `Balance` of a list of a filtered list of `Entries`.
selectEntries : (Account -> Bool) -> Vect k Entries -> Balance
selectEntries selector entries = concat $ map (select selector) entries

export
assets : Vect k Transaction -> Balance
assets = selectEntries (isA Asset) . map entries

export
liabilities : Vect k Transaction -> Balance
liabilities = selectEntries (isA Liability) . map entries

export
capital : Vect k Transaction -> Balance
capital = selectEntries (isA Equity) . map entries

export
expenses : Vect k Transaction -> Balance
expenses = selectEntries (isA Expense) . map entries

export
revenues : Vect k Transaction -> Balance
revenues = selectEntries (isA Revenue) . map entries

||| A global book of `Account`s made from a sequence of transactions
||| This type guarantees the sequence of transactions respects the _fundamental equation_
||| of accounting: ```assets + expenses = liabilities + capital + revenues```
public export
data BookOfAccounts : Type where
  BookTransactions : (txs : Vect k Transaction) ->
                     { auto fundamentalEquation : inverse (assets txs <+> expenses txs) = liabilities txs <+> capital txs <+> revenues txs } ->
                     BookOfAccounts

export
Show BookOfAccounts where
  show (BookTransactions txs) = unlines (toList $ map show txs)

export
record AccountState where
  constructor MkAccountState
  account : Account
  balance : Balance

export
Show AccountState where
  show (MkAccountState account Zero) = show account ++ " 0"
  show (MkAccountState account bal) = show account ++ " " ++ show bal

||| Compute the global balance of a `BookOfAccounts` producing a
||| a `AccountState` for each account in the ledger.
export
balances : BookOfAccounts -> List AccountState
balances (BookTransactions txs) = SMap.values $ foldl accumulate SMap.empty txs
  where
    accEntries : SMap.SortedMap Account AccountState -> Entry -> SMap.SortedMap Account AccountState
    accEntries bal (MkEntry amount direction account) =
      case SMap.lookup account bal of
        Nothing => SMap.insert account (MkAccountState account (Bal amount direction)) bal
        Just (MkAccountState account balance) =>
          let account' = MkAccountState account (balance <+> Bal amount direction)
          in SMap.insert account account' bal

    accumulate : SMap.SortedMap Account AccountState -> Transaction -> SMap.SortedMap Account AccountState
    accumulate bal (Tx label date (MkEntries entries)) = foldl accEntries bal entries


namespace BookTest
  %access private

  tx : Transaction
  tx = Tx "Some transaction" (MkDate 2019 January 01) $ MkEntries [ MkEntry 100 Debit Bank
                                                                  , MkEntry 100 Credit Capital ]

  book1 : BookOfAccounts
  book1 = BookTransactions [ tx ]
