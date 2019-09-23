||| Compute the balance of a list of transactions
module Accounting.Balance

import Accounting.Core

import Data.Vect

record AccountState where
  constructor MkAccountState
  account : Account
  balance : Balance
