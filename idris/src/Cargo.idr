module Cargo

import Data.List

%default total
-- From DDD p.17 seq

record Cargo where
  constructor MkCargo
  size : Int

implementation Eq Cargo  where
 (MkCargo s) == (MkCargo s') = s == s'

-- Borrowed technique from Reflection.idr
-- this is needed to get the right types in the proof of DecEq for Cargo, and is as usual counterintuitive
-- one is given a proof of inequality of size contra : (s = s') -> Void and we want to transform that in a proof of inequality of Cargos (MkCargo s = MkCargo s') -> Void
-- so we need a function (MkCargo s = MkCargo s') -> (s = s') that composed with contra gives the wanted function
--
-- under the interpretation of -> as logical implication, we have a A -> B and B -> C hence we can derive A -> C by transitivity of logical implication, which is the same as function composition

mutual
  implementation DecEq Cargo where
    decEq (MkCargo s) (MkCargo s') =
      case decEq s s' of
        (Yes prf)   => rewrite prf in Yes Refl
        (No contra) => No $ contra . sizeInj

  private
  sizeInj : (MkCargo s = MkCargo s') -> (s = s')
  sizeInj Refl = Refl

record Voyage where
  constructor MkVoyage
  capacity : Int
  orderConfirmation : Int
  cargos : List Cargo

-- | Book a cargo on a Voyage
makeBooking : Cargo -> Voyage -> Voyage
makeBooking cargo voyage =
  record { cargos = cargo :: cargos voyage } voyage

-- | Allow 110% overbooking
bookedCargoSize : Voyage -> Int
bookedCargoSize (MkVoyage capacity orderConfirmation cargos) = sum (map size cargos)

makeBooking' : Cargo -> Voyage -> Voyage
makeBooking' cargo@(MkCargo size) voyage@(MkVoyage capacity orderConfirmation cargos)
             =  if cast (bookedCargoSize voyage + size) > 1.1 * cast capacity
                   then  voyage
                   else record { cargos = cargo :: cargos } voyage


-- | Extract policy as a specific type, in this case an alias

||| A proof that a cargo is confirmed for given voyage
data HasCargo : (cargo : Cargo) -> (voyage : Voyage) -> Type where
  CargoConfirmed : {auto prf : Elem cargo cargos} -> HasCargo cargo (MkVoyage cap order cargos)

mutual
  hasCargo : (cargo : Cargo) -> (voyage : Voyage) -> Dec (HasCargo cargo voyage)
  hasCargo cargo (MkVoyage capacity orderConfirmation []) = No voyageIsEmpty
  hasCargo cargo (MkVoyage capacity orderConfirmation cargos) =
    case isElem cargo cargos of
      (Yes prf) => Yes CargoConfirmed
      (No contra) => No (contra . cargoConfirmed)


  voyageIsEmpty : HasCargo cargo (MkVoyage capacity orderConfirmation []) -> Void
  voyageIsEmpty CargoConfirmed impossible

  cargoConfirmed : HasCargo cargo (MkVoyage capacity orderConfirmation cargos) -> Elem cargo cargos
  cargoConfirmed (CargoConfirmed {prf}) = prf

-- data CanBookCargo : (cargo : Cargo) -> (voyage : Voyage)  -> Type where
--   CargoBooked : (cargo : Cargo) -> (voyage : Voyage) -> { auto prf : HasCargo cargo (cargos voyage) } -> CanBookCargo cargo voyage

OverbookingPolicy : Type
OverbookingPolicy = Cargo -> Voyage -> Bool

tenPercentOverbooking : OverbookingPolicy
tenPercentOverbooking cargo@(MkCargo size) voyage@(MkVoyage capacity orderConfirmation cargos) =
  cast (bookedCargoSize voyage + size) > 1.1 * cast capacity

||| Booking produces a dependent pair that associates a potentially transformed
||| voyage along with a proof that the cargo is part (or not) of the produced `voyage'`
|||
||| The idea is that we add more information to the function than a simple return value that would need to be inspected
||| at runtime: Having a proof the cargo is part of the produced voyage will help clients of this function
||| make further decisions.
makeBooking'' : (cargo : Cargo) -> Voyage
              -> OverbookingPolicy
               -> (voyage' : Voyage ** Dec (HasCargo cargo voyage'))
makeBooking'' cargo voyage@(MkVoyage capacity orderConfirmation cargos) isAllowed =
  let voyage' = if isAllowed cargo voyage
                  then MkVoyage capacity orderConfirmation (cargo :: cargos)
                  else voyage
  in (voyage' ** hasCargo cargo voyage')
