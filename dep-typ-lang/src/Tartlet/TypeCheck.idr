module Tartlet.TypeCheck

import Expr
import Env
import Value
import Eval
import AlphaEquiv

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

isEqual : Ctx -> Value -> Either Message (Ty, Value, Value)
isEqual _ (VEq ty from to) = pure (ty, from, to)
isEqual ctx other = unexpected ctx "Not an equality type" other

isAbsurd : Ctx -> Value -> Either Message ()
isAbsurd _ VAbsurd = pure ()
isAbsurd ctx other = unexpected ctx "Not Absurd: " other

isTrivial : Ctx -> Value -> Either Message ()
isTrivial _ VTrivial = pure ()
isTrivial ctx other = unexpected ctx "Not Trivial" other

isAtom : Ctx -> Value -> Either Message ()
isAtom _ VAtom = pure ()
isAtom ctx other = unexpected ctx "Not Atom" other

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
  synth ctx (Replace tgt mot base) =
    do t <- synth ctx tgt
       (ty, from, to) <- isEqual ctx t
       let motTy = eval [("ty", ty)] (Pi "x" (Var "ty") U)
       check ctx mot motTy
       let motV = eval (mkEnv ctx) mot
       check ctx base (doApply motV from)
       pure (doApply motV to)
  synth ctx Trivial = pure VU
  synth ctx Absurd = pure VU
  synth ctx (IndAbsurd tgt mot) =
    do t <- synth ctx tgt
       isAbsurd ctx t
       check ctx mot VU
       pure (eval (mkEnv ctx) mot)
  synth ctx Atom = pure VU
  synth ctx U = pure VU
  synth ctx (The ty expr) =
    do check ctx ty VU
       let tyV = eval (mkEnv ctx) ty
       check ctx expr tyV
       pure tyV

  synth _ other = Left $ "Unable to synthetize type for " ++ show other

  check : Ctx -> Expr -> Ty -> Either Message ()
  check ctx (Lam x body) t =
    do (a,b) <- isPi ctx t
       let xV = evalClosure b (VNeutral a (NVar x))
       check (extendCtx ctx x a) body xV
  check ctx (Pair a d) t =
    do (aT, dT) <- isSigma ctx t
       check ctx a aT
       let aV = eval (mkEnv ctx) a
       check ctx d (evalClosure dT aV )
  check ctx Z t = isNat ctx t
  check ctx (S n) t =
    do isNat ctx t
       check ctx n VNat
  check ctx Same t =
    do (t, from, to) <- isEqual ctx t
       convert ctx t from to
  check ctx Sole t = isTrivial ctx t
  check ctx (Tick a) t = isAtom ctx t
  check ctx other t =
    do t' <- synth ctx other
       convert ctx VU t' t

  convert : Ctx -> Ty -> Value -> Value -> Either Message ()
  convert ctx t v1 v2 =
    if alphaEquiv e1 e2
    then pure ()
    else Left (show e1 ++ " is not the same type as " ++ show e2)
    where
      e1 : Expr
      e1 = readBackTyped ctx t v1
      e2 : Expr
      e2 = readBackTyped ctx t v2
