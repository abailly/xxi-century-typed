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
import public Decidable.Equality

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

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

public export
data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

Eq Account where
  (MkAccount lbl {type=t}) == (MkAccount lbl' {type=t'}) = lbl == lbl' && t == t'

Show Account where
  show (MkAccount lbl {type=t}) = show t ++ ":" ++ lbl

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


withSameLength : (en : Vect n' Entry) -> (prf : n = n') -> Vect n Entry
withSameLength en prf = rewrite prf in en

Eq Entries where
  (MkEntries en {n=n}) == (MkEntries en' {n=n'}) with (decEq n n')
     | (Yes prf) = withSameLength en' prf == en
     | (No _)    = False

Show Entries where
  show (MkEntries ens) = unlines (toList $ map show ens)

export
record Transaction where
  constructor Tx
  label : String
  date : String
  entries : Entries

Eq Transaction where
  (Tx lbl dat en) == (Tx lbl' dat' en') = lbl == lbl' && dat == dat' && en == en'

Show Transaction where
  show (Tx lbl dt entries) = dt ++ " " ++ lbl ++ "\n" ++ show entries

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

Show BookOfAccounts where
  show (BookTransactions txs) = unlines (toList $ map show txs)

tx : Transaction
tx = Tx "Some transaction" "2019-01-01" $ MkEntries [ MkEntry (100, Dr) Bank,
                                                      MkEntry (100, Cr) Capital ]

book1 : BookOfAccounts
book1 = BookTransactions [ tx ]

ReadError : Type
ReadError = String

readDate : Parser String
readDate = do
  year <- pack <$> ntimes 4 (satisfy isDigit)
  char '-'
  month <- pack  <$> ntimes 2 (satisfy isDigit)
  char '-'
  day <- pack  <$> ntimes 2 (satisfy isDigit)
  pure $ year ++ "-" ++ month ++ "-" ++ day

readLabel : Parser String
readLabel = pack <$> manyTill anyChar (eol <|> eof)
  where
  eol : Parser ()
  eol = do
    endOfLine
    pure ()

readTxHeader : Parser (String, String)
readTxHeader = do
  date <- readDate
  spaces
  lbl <- readLabel
  pure $ (date, lbl)

readAccountType : Parser AccountType
readAccountType = do
  typ <- string "Asset" <|>
         string "Liability" <|>
         string "Equity" <|>
         string "Expense" <|>
         string "Revenue"
  case typ of
    "Asset" => pure Asset
    "Liability" => pure Liability
    "Equity" => pure Equity
    "Expense" => pure Expense
    "Revenue" => pure Revenue

readAccount : Parser Account
readAccount = do
  acctype <- readAccountType
  char ':'
  lbl <- pack <$> many alphaNum
  pure $ MkAccount lbl {type=acctype}

readDirection : Parser Direction
readDirection =
  char 'D' *> pure Dr <|> char 'C' *> pure Cr

readEntry : Parser Entry
readEntry = do
  ntimes 2 space
  acc <- readAccount
  spaces
  dir <- readDirection
  spaces
  amnt <- integer
  pure $ MkEntry (amnt, dir) acc

readEntries : Parser Entries
readEntries = do
  e1 <- readEntry
  endOfLine
  e2 <- readEntry
  endOfLine
  es <- sepBy readEntry endOfLine
  let entries = e1 :: e2 :: fromList es
  case decEq (balance entries) (Z, Cr) of
    (Yes prf) => pure $ MkEntries entries
    (No  _)   => fail "Entries are not balanced, total debits minus total credits should be 0"


||| Parse a transaction from a `String`
||| The expected format is:
||| * A line with an ISO8601-formatted data, one or more space, and a free-form label||| * 2 or more lines starting with at least 2 spaces, of the form:
|||   * Account type, followed by a colon, followed by an account name
|||     The account name can contain spaces
|||   * one or more spaces, followed by the letter D or C, followe by an integer
|||
parseTransaction : Parser Transaction
parseTransaction = do
  (dt, lbl) <- readTxHeader
  entries <- readEntries
  pure $ Tx dt lbl entries

parseBookOfAccounts : Parser BookOfAccounts
parseBookOfAccounts = do
  rawTxs <- sepBy parseTransaction (many endOfLine)
  let txs = fromList rawTxs
  case decEq (invert (assets txs)) (liabilities txs <+> capital txs) of
    (Yes prf) => pure $ BookTransactions txs
    (No contra) => fail "Transactions are not balanced, assets should equal sum of liabilities and capital"

export
readAccounts : IO ()
readAccounts = do
  [ _, file ] <- getArgs
  Right input <- readFile file
        | Left err => putStrLn ("failed to read file " ++ file ++ ": "  ++ show err)
  case parse parseBookOfAccounts input of
    Right tx => putStrLn (show tx)
    Left err => putStrLn $ "error: "  ++ err
