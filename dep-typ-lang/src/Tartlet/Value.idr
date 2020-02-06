module Tartlet.Value

import AlphaEquiv
import Expr
import Env

%access public export
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
    show (VPi ty closure) = "Π (" ++ show ty ++ "). " ++ show closure
    show (VLambda closure) = "λ " ++ show closure
    show (VSigma ty closure) = "Σ (" ++ show ty ++ "). " ++ show closure
    show (VPair value value') = "(" ++ show value ++ "," ++ show value' ++ ")"
    show (VNat) = "Nat"
    show VZ = "Z"
    show (VS value) = "S " ++ show value
    show (VEq ty value value') = "= "++ show ty ++ " "++ show value ++ " " ++ show value'
    show (VSame) = "Same"
    show (VTrivial) = "Trivial"
    show (VSole) = "Sole"
    show (VAbsurd) = "Absurd"
    show (VAtom) = "Atom"
    show (VTick string) = "'" ++ string
    show (VU) = "U"
    show (VNeutral ty neut) = "? (" ++ show neut ++ ") : " ++ show ty

  Show Closure where
    show (MkClosure env name body) = "Closure"

  Show Neutral where
    show _ = "Neutral"

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

mkEnv : Ctx -> Env Value
mkEnv [] = []
mkEnv ((x, e) :: ctx) = ((x, v) :: mkEnv ctx)
  where
    v = case e of
          Def _ v' => v'
          IsA t => VNeutral t (NVar x)
