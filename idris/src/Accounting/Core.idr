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

public export
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

-- Proofs for Group Properties of Balance
-- Recall that a Group must satisfy the following laws:
-- + Associativity of `<+>`:
--     forall a b c, a <+> (b <+> c) == (a <+> b) <+> c
-- + Neutral for `<+>`:
--     forall a,     a <+> neutral   == a
--     forall a,     neutral <+> a   == a
-- + Inverse for `<+>`:
--     forall a,     a <+> inverse a == neutral
--     forall a,     inverse a <+> a == neutral

-- Proofs for neutral are pretty trivial as they are directly reducible
-- from the definition of `<+>`
balanceRightNeutral : (b : Balance) -> (b <+> Zero = b)
balanceRightNeutral Zero      = Refl
balanceRightNeutral (Bal n d) = Refl

balanceLeftNeutral : (b : Balance) -> (Zero <+> b = b)
balanceLeftNeutral Zero      = Refl
balanceLeftNeutral (Bal n d) = Refl

-- Proof of (right) inverse is slightly more involved as we need to check the 2 cases
-- for `decEq n n` stemming from the definition of `<+>`.
-- We also need to expand the `Bal`'s amount to `MkAmount n` in order to trigger the
-- reducing of `compensate`. And for the same reason but applied to `inverse` we need
-- to pattern match on the direction too.
balanceRightInverse : (b : Balance) -> (b <+> inverse b = Zero)
balanceRightInverse Zero      = Refl
balanceRightInverse (Bal (MkAmount n) Dr) with (decEq n n)
  | (Yes prf)   = Refl
  | (No contra) = absurd (contra Refl)

balanceRightInverse (Bal (MkAmount n) Cr) with (decEq n n)
  | (Yes prf) = Refl
  | (No contra) = absurd (contra Refl)


-- Lemmas proved while trying to prove associativity
-- I gave up at some point as it became more and more tedious, with dozens of cases to unfold because
-- of the way `<+>` and `compensate` are defined.
-- I will need to think more about the types, maybe to embed more "proofs" in the types themselves to
-- limit the number of cases to reduce

||| Balances are equal when their `Amount` is made of equal `Nat`
||| This proof uses `lteUniqueProof` provided in `Amount` in order to ensure
||| the compiler has the needed implicits that n and m are greater than 0.
eqNatIsEqAmount : (n = m)
                -> {notZeron : LTE 1 n} -> {notZerom : LTE 1 m}
                -> (Bal (MkAmount n) d = Bal (MkAmount m) d)
eqNatIsEqAmount Refl {notZeron} {notZerom} =
  rewrite lteUniqueProof notZeron notZerom in Refl

||| Lift associativity of `Nat`s into `Balance`
||| This is trivial once we have `eqNatIsEqAmount` thanks to prelude's `plusAssociative`
amountAssociative : (notZeron : LTE 1 n) -> (notZerom : LTE 1 m) -> (notZerok : LTE 1 k)
                  -> { auto prf1 : LTE 1 (plus n (plus m k)) }
                  -> { auto prf2 : LTE 1 (plus (plus n m) k) }
                  -> (Bal (MkAmount (plus n (plus m k))) z = Bal (MkAmount (plus (plus n m) k)) z)
amountAssociative notZeron notZerom notZerok {n} {m} {k} =
  eqNatIsEqAmount (plusAssociative n m k)

||| A simpler version of `succInjective` from `Prelude.Nat`
succInj : (S n = S m) -> n = m
succInj Refl = Refl

||| Proof that adding a number greater than 0 to any number cannot yield 2 equal numbers
||| This looks obvious but was actually quite tricky to write. Pattern matching on the equality
||| does not work: The hole gets filled by an `impossible` statement but this is wrong. And
||| manually matching on `Refl` gives a type error
notPlusSucc : (n : Nat) -> Not (n + S m = n)
notPlusSucc Z     prf = SIsNotZ prf
notPlusSucc (S n) prf = notPlusSucc n (succInj prf)

||| A very specialised lemma to prove a `Balance` cannot be zero under some (obvious) conditions
||| The `Z` cases for each of `m` and `k` easily yield `impossible` statements.
zeroNotEqBal : (x : plus m k = m) -> (prf : plus m k = k) -> {auto notZerom : LTE 1 m} -> {notZerok : LTE 1 k} -> (Zero = Bal (MkAmount k) z)
zeroNotEqBal {m = Z}     {k}         _ _ impossible
zeroNotEqBal {m}         {k = Z}     x prf impossible
-- it's unclear to me why we need to match explicitly the k but this seems to trigger
-- the reduction of `notPlusSucc` correctly so that the types line up for `m + S k = m`.
-- I suppose the typechecker does not know at this stage the only possible value for
-- k is `S k` as the other cases are covered before?
zeroNotEqBal {m = m}     {k = (S k)} x prf = absurd (notPlusSucc m x)


||| (Partial) proof that <+> is associative
||| The cases involving `Zero`s are easy, things go awry in the general case.
balanceAssociative : (a,b,c : Balance) -> (a <+> (b <+> c) = (a <+> b) <+> c)
balanceAssociative Zero      Zero      c = Refl
balanceAssociative (Bal n d) Zero      c = Refl
balanceAssociative Zero      (Bal n d) c = Refl
balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) Zero with (decEq y d)
  | (Yes prf) = Refl
  | (No contra) with (decEq n m)
    | (Yes prf) = Refl
    | (No f) with (order {to=LTE} n m)
      | (Left l) = Refl
      | (Right r) = Refl

balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) with (decEq d z)
  balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes prf) with (decEq y d)
    balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) d) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes prf) | (Yes x) with (decEq y z)
      balanceAssociative (Bal (MkAmount n {notZero=notZeron}) z) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (Yes Refl) | (Yes Refl) =
        amountAssociative notZeron notZerom notZerok
      balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) (Bal (MkAmount k) z) | (Yes prf) | (Yes x) | (No contra) with (decEq (n + m) k)
        balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount (plus n m)) z) | (Yes Refl) | (Yes Refl) | (No contra) | (Yes Refl) = absurd (contra Refl)
        balanceAssociative (Bal (MkAmount n) y) (Bal (MkAmount m) d) (Bal (MkAmount k ) z) | (Yes prf) | (Yes x) | (No contra) | (No f) with (order {to=LTE} (n + m) k)
          balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount k) z) | (Yes Refl) | (Yes Refl) | (No contra) | (No f) | (Left l) = absurd (contra Refl)
          balanceAssociative (Bal (MkAmount n) z) (Bal (MkAmount m) z) (Bal (MkAmount k) z) | (Yes Refl) | (Yes Refl) | (No contra) | (No f) | (Right r) = absurd (contra Refl)

    balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) with (decEq n (m + k))
      balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z)     | (Yes Refl) | (No contra) | (Yes Refl) with (decEq (m + k) k)
        balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z)   | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) with (decEq (m + k) m)
          balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) | (Yes x) = zeroNotEqBal x prf
          balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (Yes prf) | (No f)  = believe_me "proof is way too involved which probably means there is something wrontg in the types"


        balanceAssociative (Bal (MkAmount (plus m k) {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (Yes Refl) | (No f) = believe_me "proof is way too involved which probably means there is something wrontg in the types"

      balanceAssociative (Bal (MkAmount n {notZero=notZeron}) y) (Bal (MkAmount m {notZero=notZerom}) z) (Bal (MkAmount k {notZero=notZerok}) z) | (Yes Refl) | (No contra) | (No f) = believe_me "proof is way too involved which probably means there is something wrontg in the types"


  | (No contra) = believe_me "proof is way too involved which probably means there is something wrontg in the types"




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

valid1 : balance [ MkEntry (MkAmount 100) Dr Bank,
                   MkEntry (MkAmount 100) Cr Capital ] = Zero
valid1 = Refl

valid2 : balance [ MkEntry (MkAmount 100) Cr Bank,
                  MkEntry (MkAmount 100) Dr Capital ] = Zero
valid2 = Refl

invalid : Not (balance [ MkEntry (MkAmount 100) Cr Bank,
                         MkEntry (MkAmount 101) Dr Capital ] = Zero)
invalid = \ Refl impossible

tx : Transaction
tx = Tx "Some transaction" (MkDate 2019 January 01) $ MkEntries [ MkEntry (MkAmount 100) Dr Bank,
                                                                  MkEntry (MkAmount 100) Cr Capital ]

book1 : BookOfAccounts
book1 = BookTransactions [ tx ]
