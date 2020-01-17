module Tartlet.Eval

import Expr
import Value
import Env

doIndNat : Value -> Value -> Value -> Value -> Value

doReplace : Value -> Value -> Value -> Value

doIndAbsurd : Value -> Value -> Value

evalVar : Env Value -> Name -> Value

doApply : Value -> Value -> Value

doCar : Value -> Value

doCdr : Value -> Value

eval : Env Value -> Expr -> Value
eval env (Var x) = evalVar env x
eval env (Pi x dom ran) = VPi (eval env dom) (MkClosure env x ran)
eval env (Lam x body)  = VLambda (MkClosure env x body)
eval env (App rator rand) = doApply (eval env rator) (eval env rand)
eval env (Sigma x carType cdrType) = VSigma (eval env carType) (MkClosure env x cdrType)
eval env Z = VZ
eval env (S e) = VS (eval env e)
eval env (IndNat tgt mot base step) = doIndNat (eval env tgt) (eval env mot) (eval env base) (eval env step)
eval env (Pair a d ) = VPair (eval env a) (eval env d)
eval env (P1 e) = doCar (eval env e)
eval env (P2 e) = doCdr (eval env e)
eval env Nat = VNat
eval env (Equal ty from to) = VEq (eval env ty) (eval env from) (eval env to)
eval env Same = VSame
eval env (Replace tgt mot base) = doReplace (eval env tgt) (eval env mot) (eval env base)
eval env Trivial = VTrivial
eval env Sole = VSole
eval env Absurd = VAbsurd
eval env (IndAbsurd tgt mot) = doIndAbsurd (eval env tgt) (eval env mot)
eval env Atom = VAtom
eval env (Tick x) = VTick x
eval env U = VU
eval env (The ty e) = eval env e
