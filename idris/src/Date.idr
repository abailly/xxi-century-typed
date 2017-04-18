module Date 

import Decidable.Order

%default total
%access public export

data Month : Type where
  January    : Month
  February   : Month
  March      : Month
  April      : Month
  May        : Month
  June       : Month
  July       : Month
  August     : Month
  September  : Month
  October    : Month
  November   : Month
  December   : Month

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

implementation Eq Month where
  m1 == m2 = toNat m1 == toNat m2

implementation Ord Month where
  compare m1  m2 = compare (toNat m1) (toNat m2)
  
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

daysInMonth : Month -> Year -> Nat
daysInMonth January _      = 31
daysInMonth February year  = if isLeapYear year then 29 else 28
daysInMonth March _        = 31
daysInMonth April _        = 30
daysInMonth May _          = 31
daysInMonth June _         = 30
daysInMonth July _         = 31
daysInMonth August _       = 31
daysInMonth September _    = 30
daysInMonth October _      = 31
daysInMonth November _     = 30
daysInMonth December _     = 31

aMonthHasOneDay : (month : Month) -> (year : Year) -> LTE 1 (daysInMonth month year)
aMonthHasOneDay January year   = LTESucc LTEZero
aMonthHasOneDay February year  = case daysInMonth February year of 
                                      val => ?hole
aMonthHasOneDay March year     = LTESucc LTEZero
aMonthHasOneDay April year     = LTESucc LTEZero
aMonthHasOneDay May year       = LTESucc LTEZero
aMonthHasOneDay June year      = LTESucc LTEZero
aMonthHasOneDay July year      = LTESucc LTEZero
aMonthHasOneDay August year    = LTESucc LTEZero
aMonthHasOneDay September year = LTESucc LTEZero
aMonthHasOneDay October year   = LTESucc LTEZero
aMonthHasOneDay November year  = LTESucc LTEZero
aMonthHasOneDay December year  = LTESucc LTEZero

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
         -> Date

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


-- implementation Eq Date where
--   (MkDate y1 m1 d1) == (MkDate y2 m2 d2) = 
--     d1 == d2 && m1 == m2 && y1 == y2
    
-- implementation Ord Date where
--   compare (MkDate d1 m1 y1) (MkDate d2 m2 y2) =
--     case compare y1 y2 of
--       EQ => case compare m1 m2 of
--                  EQ => compare d1 d2
--                  LT => LT
--                  GT => GT
--       LT => LT
--       GT => GT

-- addDays : Date -> Nat -> Date
-- addDays (MkDate d m y) days =
--   let maxDays = daysInMonth m y
--       shiftedDays = finToNat d + days
--   in case natToFin shiftedDays (finToNat maxDays) of  
--           Nothing => ?hole_1
--           Just x  => MkDate x m y
     
