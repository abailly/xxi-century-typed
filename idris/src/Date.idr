||| Date representation in Gregorian Calendar
|||
||| This module provides basic `Date` type encoded as dates in the Gregorian
||| calendar, with an [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) `String`
||| representation. The `Date`'s only constructor `MkDate` statically ensures
||| the constructed date is valid, eg. it is a type error to try to define a
||| `MkDate 2019 February 29` as 2019 is not a bisextile year.
module Date

import Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Decidable.Order
import Data.String

%default total
%access public export

data Month =
 January | February | March | April | May | June |
 July | August | September | October | November | December

toNat : Month -> Nat
toNat January    = 1
toNat February   = 2
toNat March      = 3
toNat April      = 4
toNat May        = 5
toNat June       = 6
toNat July       = 7
toNat August     = 8
toNat September  = 9
toNat October    = 10
toNat November   = 11
toNat December   = 12

Eq Month where
  m1 == m2 = toNat m1 == toNat m2

Ord Month where
  compare m1  m2 = compare (toNat m1) (toNat m2)

fromInteger : Integer -> Either String Month
fromInteger 1 = Right January
fromInteger 2 = Right February
fromInteger 3 = Right March
fromInteger 4 = Right April
fromInteger 5 = Right May
fromInteger 6 = Right June
fromInteger 7 = Right July
fromInteger 8 = Right August
fromInteger 9 = Right September
fromInteger 10 = Right October
fromInteger 11 = Right November
fromInteger 12 = Right December
fromInteger n = Left $ "Invalid month number " ++ show n

Year : Type
Year = Nat

isLeapYear : Year -> Bool
isLeapYear y = check4 && check100 || check400
  where
    check4 : Bool
    check4 = modNatNZ y 4 SIsNotZ == 0

    check100 : Bool
    check100 = modNatNZ y 100 SIsNotZ /= 0

    check400 : Bool
    check400 = modNatNZ y 400 SIsNotZ == 0

monthDuration : Month -> Year -> (days: Nat ** LTE 1 days)
monthDuration January _      = (31 ** LTESucc LTEZero)
monthDuration February year  = if isLeapYear year
                               then (29  ** LTESucc LTEZero)
                               else (28 ** LTESucc LTEZero)
monthDuration March _        = (31 ** LTESucc LTEZero)
monthDuration April _        = (30 ** LTESucc LTEZero)
monthDuration May _          = (31 ** LTESucc LTEZero)
monthDuration June _         = (30 ** LTESucc LTEZero)
monthDuration July _         = (31 ** LTESucc LTEZero)
monthDuration August _       = (31 ** LTESucc LTEZero)
monthDuration September _    = (30 ** LTESucc LTEZero)
monthDuration October _      = (31 ** LTESucc LTEZero)
monthDuration November _     = (30 ** LTESucc LTEZero)
monthDuration December _     = (31 ** LTESucc LTEZero)

daysInMonth : Month -> Year -> Nat
daysInMonth month year with (monthDuration month year)
  | (days ** _) = days

aMonthHasOneDay : (month : Month) -> (year : Year) -> LTE 1 (daysInMonth month year)
aMonthHasOneDay month year with (monthDuration month year)
  | (_ ** prf) = prf

nextMonth : Month -> Month
nextMonth January   = February
nextMonth February  = March
nextMonth March     = April
nextMonth April     = May
nextMonth May       = June
nextMonth June      = July
nextMonth July      = August
nextMonth August    = September
nextMonth September = October
nextMonth October   = November
nextMonth November  = December
nextMonth December  = January

data Date : Type where
  MkDate : (year  : Year) -> (month : Month ) -> (day : Nat)
         -> { auto dayFitInMonth : LTE day (daysInMonth month year) }
         -> { auto dayGreaterThanOne : LTE 1 day }
         -> Date


Eq Date where
  (MkDate y1 m1 d1) == (MkDate y2 m2 d2) =
    d1 == d2 && m1 == m2 && y1 == y2

Ord Date where
  compare (MkDate d1 m1 y1) (MkDate d2 m2 y2) =
    case compare y1 y2 of
      EQ => case compare m1 m2 of
                 EQ => compare d1 d2
                 LT => LT
                 GT => GT
      LT => LT
      GT => GT

Show Date where
  show (MkDate y m d) = show y ++ "-" ++ with2Digits (toNat m) ++ "-" ++ with2Digits d
  where
    with2Digits : Nat -> String
    with2Digits n = if n <= 9
                    then "0" ++ show n
                    else show n

daysMax : (d: Date) -> Nat
daysMax (MkDate y m _) = daysInMonth m y


addOneDay : (d : Date) -> Date
addOneDay (MkDate year month day) =
  case order {to=LTE} (S day) (daysInMonth month year) of
    Left _  =>
      -- easy case: simply add one day
      MkDate year month (S day)
    Right _ =>
      case month of
        -- We need to roll by one year
        December => MkDate (year + 1) January 1
          -- We need to roll by one month
        _        => let firstDayOfMonth = aMonthHasOneDay (nextMonth month) year
                    in MkDate year (nextMonth month) 1

addDays : (d : Date)
        -> (n : Nat)
        -> Date
addDays d Z     = d
addDays d (S k) = addDays (addOneDay d) k


mkYear : (y : String) -> Either String Year
mkYear y =
  case parsePositive {a=Nat} y of
    Nothing => Left ("Invalid year " ++ y)
    (Just x) => Right x

mkMonth : (y : String) -> Either String Month
mkMonth y =
  case parsePositive {a=Integer} y of
    Nothing => Left ("Invalid month " ++ y)
    (Just x) => fromInteger x

mkDay : (y : String) -> Either String Nat
mkDay y =
  case parsePositive {a=Nat} y of
    Nothing => Left ("Invalid day " ++ y)
    (Just x) => Right x

toDate : String -> String -> String -> Either String Date
toDate y m d = do
  year <- mkYear y
  month <- mkMonth m
  day <- mkDay d
  case isLTE day (daysInMonth month year) of
    (Yes prf) => case isLTE 1 day of
                      (Yes prf') => pure $ MkDate year month day
                      (No _) => Left ("invalid number of days " ++ d ++ " in month " ++ m ++ " and year " ++ y)
    (No _) => Left ("invalid number of days "++ d ++ " in month " ++ m ++ " and year " ++ y)

||| Parse a `Date` in ISO8601 format
||| Date must respect the format `YYYY-mm-dd` and of course be a valid date.
parseISO8601Date : Parser Date
parseISO8601Date = do
  let digits = \ n => pack <$> ntimes n (satisfy isDigit)
  y <- digits 4
  char '-'
  m <- digits 2
  char '-'
  d <- digits 2
  either fail pure (toDate y m d)
