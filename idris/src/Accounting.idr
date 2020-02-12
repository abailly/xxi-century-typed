||| # Dependently-Typed Accounting
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
||| Tx "Sell 1 Brouzouf" "1/1/2019" $ MkEntries [ MkEntry (100, Debit) Customer,
|||                                               MkEntry (100, Credit) Sale ]
||| ```
||| eg. we increase our revenue by 100 and we record the fact the customer has
||| a debt of 100.
||| Then when the customer pays, we get a bank transaction that balances the
||| customer's account
||| ```
||| Bank = Account "Bank" {type=Asset}
|||
||| Tx "Wire transfer" "2/1/2019" $ MkEntries [ MkEntry (100, Credit) Customer,
|||                                             MkEntry (100, Debit) Bank ]
||| ```
module Accounting

import public Accounting.Book
import public Accounting.Core
import Accounting.Parse
import Lightyear.Strings

export
readAccounts : IO ()
readAccounts = do
  [ _, file, action ] <- getArgs
  Right input <- readFile file
        | Left err => putStrLn ("failed to read file " ++ file ++ ": "  ++ show err)
  case parse parseBookOfAccounts input of
    Right tx => case action of
                  "register" => putStrLn (show tx)
                  "balance" => putStrLn $ unlines $ map show (balances tx)
    Left err => putStrLn $ "error: "  ++ err
