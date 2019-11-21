module TypeCheck

import Expr

%default covering

-- Type Checking
Context : Type
Context = Env Ty

initContext : Context
initContext = []

mutual

  synth : Context -> Expr -> Either Message Ty
  synth ctx (Var x) = lookupVar ctx x
  synth ctx (App fun arg) = do
    fty <- synth ctx fun
    case fty of
      (TArr dom cod) => do
        check ctx arg dom
        Right cod
      other => failure $ "not a function type: " ++ show other
  synth ctx (Rec ty tgt base step) = do
    tgtTy <- synth ctx tgt
    case tgtTy of
      TNat => do
        check ctx base tgtTy
        check ctx step (TArr TNat (TArr ty ty))
        Right ty
      other => failure $ "Not Nat type: " ++ show other
  synth ctx (Ann expr ty) = do
    check ctx expr ty
    Right ty
  synth _ other = failure $ "can't infer type for expression " ++ show other

  check : Context -> Expr -> Ty -> Either Message ()
  check ctx (Lam x y) (TArr dom cod) =  check (extend ctx x dom) y cod
  check ctx (Lam x y) ty = failure $ "Lambda should have function type: " ++ show ty
  check ctx Zero TNat = Right ()
  check ctx Zero ty = failure "Zero must have type Nat"
  check ctx (Add1 x) TNat = check ctx x TNat
  check ctx (Add1 x) ty = failure "Add1 argument must have type Nat"
  check ctx expr ty = do
    ty' <- synth ctx expr
    if ty == ty'
      then Right ()
      else failure $ "expected " ++ show ty ++" but found " ++ show ty'

addDefs : Context -> List (Name, Expr) -> Either Message Context
addDefs ctx [ ] = Right ctx
addDefs ctx ((x, e) :: defs) = do
  t <- synth ctx e
  addDefs (extend ctx x t) defs

testTypeChecking : Either Message (Ty, Ty)
testTypeChecking = do
  ctx <- addDefs initContext [ ("two",
                                (Ann (Add1 (Add1 Zero)) TNat))
                             , ("three",
                                 (Ann (Add1 (Add1 (Add1 Zero)))
                                   TNat))
                             , ("+",
                                 (Ann
                                   (Lam "n"
                                     (Lam "k"
                                       (Rec TNat
                                         (Var "n")
                                         (Var "k")
                                         (Lam "pred"
                                           (Lam "almostSum"
                                             (Add1 (Var "almostSum")))))))
                                   (TArr TNat (TArr TNat TNat))))]
  t1 <- synth ctx (App (Var "+") (Var "three"))
  t2 <- synth ctx (App (App (Var "+") (Var "three")) (Var "two"))
  Right (t1, t2)
