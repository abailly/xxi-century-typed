module Specification

import Date

-- from ch.9 of the blue book, pp.224 seq
-- 3 purposes:
--  1. to validate an object to see if it fulfills some need 
--  2. to select an object from a collection
--  3. to specify the creation of new object to fit some need

||| A specification is basically a predicate that applies to some object
interface Specification spec ty where
  validate : spec -> ty -> Bool

||| A invoice is a delinquent invoice iff the `currentDate` exceeds the `Invoice`s date taking into
||| account some `gracePeriod` which depends on the customer
record DelinquentInvoice where
  constructor MkDelinquentInvoice
  currentDate : Date

record Customer where
  constructor MkCustomer
  paymentGracePeriod : Nat
  
record Invoice where
  constructor MkInvoice 
  amount : Int
  dueDate : Date
  customer : Customer

implementation Specification DelinquentInvoice Invoice where
  validate (MkDelinquentInvoice cur) (MkInvoice am due cust) = 
    cur > (due `addDays` paymentGracePeriod cust)
