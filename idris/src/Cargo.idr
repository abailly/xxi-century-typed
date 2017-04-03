module Cargo

%default total
-- From DDD p.17 seq

record Cargo where
  constructor MkCargo
  size : Int

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

-- | Extract policy as a specific type or in this case alias
OverbookingPolicy : Type 
OverbookingPolicy = Cargo -> Voyage -> Bool

tenPercentOverbooking : OverbookingPolicy
tenPercentOverbooking (MkCargo size) voyage@(MkVoyage capacity orderConfirmation cargos) = 
      cast (bookedCargoSize voyage + size) > 1.1 * cast capacity

makeBooking'' : Cargo -> Voyage -> OverbookingPolicy -> Voyage
makeBooking'' cargo voyage isAllowed = 
  if isAllowed cargo voyage 
    then record { cargos = cargo :: cargos voyage } voyage 
    else voyage
