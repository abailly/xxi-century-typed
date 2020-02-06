module Tartlet.TypeCheck

import Expr
import Env
import Value
import Eval

%default covering

unexpected : Ctx -> String -> Value -> Either Message a
unexpected ctx msg t = Left (msg ++ ": " ++ show e)
  where
    e : Expr
    e = readBackTyped ctx VU t

isPi : Ctx -> Value -> Either Message (Ty, Closure)
isPi _ (VPi a b) = pure (a, b)
isPi ctx other = unexpected ctx "Not a Pi type" other

isSigma : Ctx -> Value -> Either Message (Ty, Closure)
isSigma _ (VSigma a b) = pure (a, b)
isSigma ctx other = unexpected ctx "Not a Sigma type" other

isNat : Ctx -> Value -> Either Message ()
isNat _ VNat = pure ()
isNat ctx other = unexpected ctx "Not Nat" other

mutual
  synth : Ctx -> Expr -> Either Message Ty
  synth ctx (Var x) =
    do t <- lookupType ctx x
       pure t
  synth ctx (Pi x a b) =
    do check ctx a VU
       check (extendCtx ctx x (eval (mkEnv ctx) a)) b VU
       pure VU
  synth ctx (App rator rand) =
    do funTy <- synth ctx rator
       (a, b) <- isPi ctx funTy
       check ctx rand a
       pure (evalClosure b (eval (mkEnv ctx) rand))
  synth ctx (Sigma x a b) =
    do check ctx a VU
       check (extendCtx ctx x (eval (mkEnv ctx) a)) b VU
       pure VU
  synth ctx (P1 e) =
    do t <- synth ctx e
       (aT, dT) <- isSigma ctx t
       pure aT
  synth ctx (P2 e) =
    do t <- synth ctx e
       (aT, dT) <- isSigma ctx t
       pure (evalClosure dT (doCar (eval (mkEnv ctx) e)))
  synth ctx (IndNat tgt mot base step) =
    do t <- synth ctx tgt
       isNat ctx t
       let tgtV = eval (mkEnv ctx) tgt
       let motTy = eval [] (Pi "x" Nat U)
       check ctx mot motTy
       let motV = eval (mkEnv ctx) mot
       check ctx base (doApply motV VZ)
       check ctx step (indNatStepType motV )
       pure (doApply motV tgtV)
  synth ctx (Equal ty from to) =
    do check ctx ty VU
       let tyV = eval (mkEnv ctx) ty
       check ctx from tyV
       check ctx to tyV
       pure VU

  synth _ _ = Left "error"

  check : Ctx -> Expr -> Ty -> Either Message ()
