module Accounting.Parse

import public Accounting.Amount
import public Accounting.Core
import Date

import Control.Algebra
import Decidable.Order
import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

ReadError : Type
ReadError = String

parseLabel : Parser String
parseLabel = pack <$> manyTill anyChar (eol <|> eof)
  where
  eol : Parser ()
  eol = do
    endOfLine
    pure ()

parseTxHeader : Parser (String, Date)
parseTxHeader = do
  date <- parseISO8601Date
  spaces
  lbl <- parseLabel
  pure $ (lbl, date)

parseAccountType : Parser AccountType
parseAccountType = do
  typ <- string "Asset"     <|>
         string "Liability" <|>
         string "Equity"    <|>
         string "Expense"   <|>
         string "Revenue"
  case typ of
    "Asset" => pure Asset
    "Liability" => pure Liability
    "Equity" => pure Equity
    "Expense" => pure Expense
    "Revenue" => pure Revenue

parseAccount : Parser Account
parseAccount = do
  acctype <- parseAccountType
  char ':'
  lbl <- pack <$> many alphaNum
  pure $ MkAccount lbl {type=acctype}

parseDirection : Parser Direction
parseDirection =
  char 'D' *> pure Dr <|> char 'C' *> pure Cr

parseEntry : Parser Entry
parseEntry = do
  ntimes 2 space
  acc <- parseAccount
  spaces
  dir <- parseDirection
  spaces
  amnt <- integer
  case order {to=LTE} 1 amnt of
    (Left l) => pure $ MkEntry (MkAmount amnt) dir acc
    (Right r) => fail "Entry's amount cannot be 0"

parseEntries : Parser Entries
parseEntries = do
  e1 <- parseEntry
  endOfLine
  e2 <- parseEntry
  endOfLine
  es <- sepBy parseEntry endOfLine
  let entries = e1 :: e2 :: fromList es
  case decEq (balance entries) Zero of
    (Yes prf) => pure $ MkEntries entries
    (No  _)   => fail "Entries are not balanced, total debits minus total credits should be 0"


||| Parse a transaction from a `String`
||| The expected format is:
||| * A line with an ISO8601-formatted data, one or more space, and a free-form label||| * 2 or more lines starting with at least 2 spaces, of the form:
|||   * Account type, followed by a colon, followed by an account name
|||     The account name can contain spaces
|||   * one or more spaces, followed by the letter D or C, followed by an integer
|||
parseTransaction : Parser Transaction
parseTransaction = do
  (dt, lbl) <- parseTxHeader
  entries <- parseEntries
  pure $ Tx dt lbl entries

||| Parse a list of transactions from `String`
||| Each transaction must be separated by one or more empty lines.
export
parseBookOfAccounts : Parser BookOfAccounts
parseBookOfAccounts = do
  rawTxs <- sepBy parseTransaction (many endOfLine)
  let txs = fromList rawTxs
  case decEq (inverse (assets txs <+> expenses txs)) (liabilities txs <+> capital txs <+> revenues txs) of
    (Yes prf) => pure $ BookTransactions txs
    (No contra) => fail "Transactions are not balanced, assets should equal sum of liabilities and capital"
