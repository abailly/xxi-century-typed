||| From Slack ADL: https://artisans-du-logiciel.slack.com/files/UA67NMTA6/FH0UVDUAX/tarif_parking.jpg
||| Goal is to "embed" the business rules for computing parking durations inside Idris,
||| computing a pricing expression as the type of a given duration

module Parking

import Data.Fin
import Data.List
import Data.Nat.DivMod

mutual

  ||| The type of correct prices for a given duration
  ||| duration is given in minutes, price in increments of 10 cents
  data HasCorrectPrice : (price : Nat) -> (duration : Nat) -> Type where

    ||| Parking is free under 10 minutes
    FreeParking : {auto prf : LTE duration 10} -> HasCorrectPrice 0 duration

    ||| Under 15 minutes, price is 60 cents
    FirstQuarter : {auto gt10 : LT 10 duration}
                 -> {auto prf : LTE duration 15}
                 -> HasCorrectPrice 6 duration

    ||| Under 1 1/2 hour, price is 30 cents per 1/4 hours
    Before1h30 : {auto gt15 : LT 15 duration}
               -> {auto prf : LTE duration 90}
               -> {auto acc : HasCorrectPrice n 15}
               -> HasCorrectPrice (n + (((-) duration 15 {smaller= lteSuccLeft gt15}) `div` 15 * 3)) duration

    ||| Under 10h 45min, price is 40 cents per 1/4 hour
    Before10h45 : {auto gt90 : LT 90 duration}
                -> {auto prf : LTE duration 645}
                -> {acc : HasCorrectPrice n 90}
                -> HasCorrectPrice (n + (((-) duration 90 {smaller = lteSuccLeft gt90}) `div` 15 * 4)) duration

    ||| Under 11 hours price is 10 cents per 1/4 hour
    Before11h00 : {auto gt10h45 : LT 645 duration}
                -> {auto prf : LTE duration 660}
                -> {acc : HasCorrectPrice n 645}
                -> HasCorrectPrice (n + 1) duration

    ||| Under 24 hours, price stays the same as for 11hours
    Before24h : {auto gt11h : LT 660 duration}
              -> {acc : HasCorrectPrice n 660}
              -> HasCorrectPrice n duration

    ||| For each day, price is the price of the accrued fraction of the day plus a full day's price
    After24h : {auto gt24h : LT 1440 duration}
             -> { auto acc1 : HasCorrectPrice m 1440 }
             -> { auto acc : HasCorrectPrice n ((-) duration 1440 { smaller = lteSuccLeft gt24h}) }
             -> HasCorrectPrice (n + m) duration

  fullDayPrice : {auto gt660 : Not (LTE k 660)} -> (price : Nat ** HasCorrectPrice price k)
  fullDayPrice {gt660} {k} = case isLTE k 1440 of
                         (Yes prf) => let (_ ** acc) = makePricing 660
                                      in (_ ** Before24h {gt11h = notLTImpliesGTE (gt660 . fromLteSucc)} {acc=acc} )
                         (No contra) => let prf1 = notLTImpliesGTE (contra . fromLteSucc)
                                            (_ ** acc1) = makePricing 1440
                                            (_ ** acc) = makePricing ((-) k 1440 { smaller = lteSuccLeft prf1 })
                                        in (_ ** After24h {acc1=acc1} {acc=acc})

  makePricing : (duration : Nat) -> (price : Nat ** HasCorrectPrice price duration)
  makePricing k =
    case (isLTE k 10) of
      (Yes prf) => (_ ** FreeParking)
      (No contra) => case isLTE k 15 of
                     (Yes prf) => (_ ** FirstQuarter {gt10 = notLTImpliesGTE (contra . fromLteSucc)})
                     (No contra) => case isLTE k 90 of
                            (Yes prf) => let (_ ** acc) = makePricing 15
                                         in (_ ** Before1h30 {gt15 = notLTImpliesGTE (contra . fromLteSucc)} {acc=acc})
                            (No contra) => case isLTE k 645 of
                                                (Yes prf) => let (_ ** acc) = makePricing 90
                                                             in (_ ** Before10h45 {gt90 = notLTImpliesGTE (contra . fromLteSucc)} {acc=acc})
                                                (No contra) => case isLTE k 660 of
                                                                    (Yes prf) => let (_ ** acc) = makePricing 645
                                                                                 in (_ ** Before11h00 {gt10h45 = notLTImpliesGTE (contra . fromLteSucc)} {acc=acc})
                                                                    (No contra) => fullDayPrice { gt660 = contra }

thePrice : Nat -> Nat
thePrice duration with (makePricing duration)
  thePrice duration | (p ** _) = p
