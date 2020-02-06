module Tartlet.Eval

import Expr
import Value
import Env

%access public export
%default covering

mutual

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

  evalClosure : Closure -> Value -> Value
  evalClosure (MkClosure env x e) v = eval (extend env x v) e

  ||| The type of induction step
  indNatStepType : Value -> Value
  indNatStepType mot =
    eval ([("mot", mot)]) (Pi "n-1" Nat
                              (Pi "almost" (App (Var "mot") (Var "n-1"))
                                (App (Var "mot")
                                  (S (Var "n-1")))))

  doIndNat : Value -> Value -> Value -> Value -> Value
  doIndNat VZ mot base step = base
  doIndNat (VS v) mot base step = doApply (doApply step v) (doIndNat v mot base step)
  doIndNat tgt@(VNeutral VNat neu) mot base step =
    VNeutral (doApply mot tgt) (NIndNat neu
                                        (MkNormal (VPi VNat (MkClosure [] "k" U)) mot) (MkNormal (doApply mot VZ) base)
                                          (MkNormal (indNatStepType mot) step))
  doIndNat _ _ _ _ = ?unimplemented

  doReplace : Value -> Value -> Value -> Value
  doReplace VSame mot base = base
  doReplace (VNeutral (VEq ty from to) neu) mot base =
    VNeutral (doApply mot to) (NReplace neu (MkNormal motT mot) (MkNormal baseT base))
    where
      motT = VPi ty (MkClosure initialEnv "x" U)
      baseT = doApply mot from
  doReplace _ _ _ = ?unimplemented

  doIndAbsurd : Value -> Value -> Value
  doIndAbsurd (VNeutral VAbsurd neu) mot = VNeutral mot (NIndAbsurd neu (MkNormal VU mot))
  doIndAbsurd _ _ = ?unimplemented

  evalVar : Env Value -> Name -> Value

  doCar : Value -> Value
  doCar (VPair v1 v2) = v1
  doCar (VNeutral (VSigma aT dT) neu) = VNeutral aT (NP1 neu)
  doCar _ = ?unimplented

  doCdr : Value -> Value
  doCdr (VPair v1 v2) = v2
  doCdr v@(VNeutral (VSigma aT dT) neu) = VNeutral (evalClosure dT (doCar v)) (NP2 neu)
  doCdr _ = ?unimplemented

  doApply : Value -> Value -> Value
  doApply (VLambda closure) arg = evalClosure closure arg
  doApply (VNeutral (VPi dom ran) neu) arg = VNeutral (evalClosure ran arg) (NApp neu (MkNormal dom arg))
  doApply _ _ = ?unimplemented


mutual

  readBackNormal : Ctx -> Normal -> Expr
  readBackNormal ctx (MkNormal t v) = readBackTyped ctx t v

  readBackTyped : Ctx -> Ty -> Value -> Expr
  readBackTyped ctx VNat VZ = Z
  readBackTyped ctx VNat (VS v) = S (readBackTyped ctx VNat v)
  readBackTyped ctx (VPi dom ran) fun =
    Lam x (readBackTyped (extendCtx ctx x dom) (evalClosure ran xVal) (doApply fun xVal))
    where
      x = freshen (ctxNames ctx) (closureName ran)
      xVal = VNeutral dom (NVar x)

  readBackTyped ctx (VSigma aT dT) pair =
    Pair (readBackTyped ctx aT carVal)  (readBackTyped ctx (evalClosure dT carVal) cdrVal)
      where
        carVal = doCar pair
        cdrVal = doCdr pair

  readBackTyped ctx VTrivial val = Sole
  readBackTyped ctx VAbsurd (VNeutral VAbsurd neu) =
    The Absurd (readBackNeutral ctx neu)
  readBackTyped ctx (VEq ) VSame = Same
  readBackTyped ctx VAtom (VTick x) = Tick x
  readBackTyped ctx VU VNat = Nat
  readBackTyped ctx VU VAtom = Atom
  readBackTyped ctx VU VTrivial = Trivial
  readBackTyped ctx VU VAbsurd = Absurd
  readBackTyped ctx VU (VEq t from to) =
    Equal (readBackTyped ctx VU t) (readBackTyped ctx t from) (readBackTyped ctx t to)
  readBackTyped ctx VU (VSigma aT dT) = Sigma x a d
    where
      x = freshen (ctxNames ctx) (closureName dT)
      a = readBackTyped ctx VU aT
      d = readBackTyped (extendCtx ctx x aT)
                        VU
                        (evalClosure dT (VNeutral aT (NVar x)))
  readBackTyped ctx VU (VPi aT bT) = Pi x a b
    where
      x = freshen (ctxNames ctx) (closureName bT)
      a = readBackTyped ctx VU aT
      b = readBackTyped (extendCtx ctx x aT)
                        VU
                        (evalClosure bT (VNeutral aT (NVar x)))
  readBackTyped ctx VU VU = U
  readBackTyped ctx t (VNeutral t' neu) = readBackNeutral ctx neu
  readBackTyped _ _ _ = ?unimplemented

  readBackNeutral : Ctx -> Neutral -> Expr
  readBackNeutral ctx (NVar x) = Var x
  readBackNeutral ctx (NApp neu arg) =
    App (readBackNeutral ctx neu) (readBackNormal ctx arg)
  readBackNeutral ctx (NP1 neu) = P1 (readBackNeutral ctx neu)
  readBackNeutral ctx (NP2 neu) = P2 (readBackNeutral ctx neu)
  readBackNeutral ctx (NIndNat neu mot base step) =
    IndNat (readBackNeutral ctx neu) (readBackNormal ctx mot) (readBackNormal ctx base) (readBackNormal ctx step)
  readBackNeutral ctx (NReplace neu mot base) =
    Replace (readBackNeutral ctx neu) (readBackNormal ctx mot) (readBackNormal ctx base)
  readBackNeutral ctx (NIndAbsurd neu mot) =
    IndAbsurd (The Absurd (readBackNeutral ctx neu)) (readBackNormal ctx mot)
