module TypeCheck

import Expr

%default covering

-- Type Checking
Context : Type
Context = Env Ty

initContext : Context
initContext = []

first : (a -> b) -> (a,c) -> (b,c)
first f (a,b) = (f a,b)

second : (a -> b) -> (c, a) -> (c, b)
second f (a, b) = (a, f b)

defsToContext : Defs -> Context
defsToContext = map (second normalType)

defsToEnv : Defs -> Env Value
defsToEnv = map (second normalValue)

mutual

  synth : Context -> Expr -> Either Message Ty
  synth ctx Zero = pure TNat
  synth ctx (Add1 x) = do
    check ctx x TNat
    pure TNat
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
  synth ctx other = failure $ "can't infer type for expression " ++ show other ++ ", context: " ++ show ctx

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

normWithDefs : Defs -> Expr -> Either Message Normal
normWithDefs defs e = do
  ty <- synth (defsToContext defs) e
  MkNormal ty <$> eval (defsToEnv defs) e

addDefs : Defs -> List (Name, Expr) -> Either Message Defs
addDefs defs [ ] = Right defs
addDefs defs ((x, e) :: more) = do
  norm <- normWithDefs defs e
  addDefs (extend defs x norm) more

definedNames : Defs -> List Name
definedNames = map fst

globalDefs : Either Message Defs
globalDefs = addDefs noDefs
                    [ ("two",
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

testTypeChecking : Expr -> Defs -> Either Message Expr
testTypeChecking e defs = do
  norm <- normWithDefs defs e
  readBackNormal (definedNames defs) norm
