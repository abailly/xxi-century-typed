module Tartlet.Value

import AlphaEquiv
import Expr
import Env

%default covering

mutual
  Ty : Type
  Ty = Value

  data Value
    = VPi Ty Closure
    | VSigma Ty Closure
    | VLambda Closure
    | VPair Value Value
    | VNat
    | VZ
    | VS Value
    | VEq Ty Value Value
    | VSame
    | VTrivial
    | VSole
    | VAbsurd
    | VAtom
    | VTick String
    | VU
    | VNeutral Ty Neutral

  data Neutral
    = NVar Name
    | NApp Neutral Normal
    | NP1 Neutral
    | NP2 Neutral
    | NIndNat Neutral Normal Normal Normal
    | NReplace Neutral Normal Normal
    | NIndAbsurd Neutral Normal

  data Normal = MkNormal Ty Value

  record Closure where
    constructor MkClosure
    closureEnv : Env Value
    closureName : Name
    closureBody : Expr

Show Value where
  show (VPi ty closure) = ?hole
  show (VLambda closure) = ?hole
  show (VSigma ty closure) = ?hole
  show (VPair value value') = ?hole
  show (VNat) = ?hole
  show VZ = ?hole
  show (VS value) = ?hole
  show (VEq ty value value') = ?hole
  show (VSame) = ?hole
  show (VTrivial) = ?hole
  show (VSole) = ?hole
  show (VAbsurd) = ?hole
  show (VAtom) = ?hole
  show (VTick string) = ?hole
  show (VU) = ?hole
  show (VNeutral ty Neutral) = ?hole

data CtxEntry = Def Ty Value | IsA Ty

Ctx : Type
Ctx = Env CtxEntry

extendCtx : Ctx -> Name -> Ty -> Ctx
extendCtx ctx x t = (x, IsA t) :: ctx

define : Ctx -> Name -> Ty -> Value -> Ctx
define ctx x t v = ((x, Def t v) :: ctx)

lookupType : Ctx -> Name -> Either Message Ty
lookupType []              x = Left ("Unbound variable: " ++ show x)
lookupType ((y, e) :: ctx) x =
  if x == y
  then case e of
         Def t _ => pure t
         IsA t =>  pure t
  else lookupType ctx x
