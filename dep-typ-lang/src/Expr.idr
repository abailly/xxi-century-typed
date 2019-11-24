module Expr

%access public export

||| Identifiers
Name : Type
Name = String

nextName : Name -> Name
nextName x = x ++ "'"

freshen : List Name -> Name -> Name
freshen used x =
  if x `elem` used
  then freshen used (nextName x)
  else x

||| Environments
Env : Type -> Type
Env val = List (Name, val)

initialEnv : Env val
initialEnv = []

-- Error handling
Message : Type
Message = String

failure : String -> Either Message a
failure = Left

lookupVar : Env val -> Name -> Either Message val
lookupVar [] name = failure $ "Variable not found: " ++ name
lookupVar ((n, v) :: env') name =
  if n == name
  then Right v
  else lookupVar env' name

extend : Env val -> Name -> val -> Env val
extend env name val = (name, val) :: env

mutual

  ||| Basic (untyped) λ-calculus expressions
  data Expr : Type where
    Var : Name -> Expr
    Lam : Name -> Expr -> Expr
    App : Expr -> Expr -> Expr
    -- builtin numbers
    Zero : Expr
    Add1 : Expr -> Expr
    -- primitive recursion
    Rec : Ty -> (tgt : Expr) -> (base : Expr) -> (step : Expr) -> Expr
    -- type annotation
    Ann : Expr -> Ty -> Expr

  ||| Types
  data Ty : Type where
    -- primitive Natural types
    TNat : Ty
    -- function types
    TArr : (dom : Ty) -> (cod : Ty) -> Ty

  Show Expr where
    show (Var x) = x
    show (Lam x y) = "λ " ++ show x ++ "." ++ show y
    show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show Zero = "0"
    show (Add1 x) = "(" ++ show x ++ " + 1)"
    show (Rec x tgt base step) = ""
    show (Ann x y) = show x ++ " : " ++ show y

  Show Ty where
    show TNat = "Nat"
    show (TArr dom cod) = show dom ++ " -> " ++ show cod

  Eq Ty where
    TNat == TNat = True
    (TArr x y) == (TArr x' y') = x == x' && y == y'
    _ == _ = False


mutual

  record Normal where
    constructor MkNormal
    normalType : Ty
    normalValue : Value

  ||| Neutral values are either free variables or application of neutral function
  ||| to a `Value`
  data Neutral : Type where
    NVar : Name -> Neutral
    NApp : Neutral -> Normal -> Neutral
    NRec : Ty -> Neutral -> Normal -> Normal -> Neutral

  ||| Values are `Expr` closed over the environment in which they are evaluated
  data Value : Type where
    VZero : Value
    VAdd1 : Value -> Value
    VClosure : (Env Value) -> Name -> Expr -> Value
    VNeutral : Ty -> Neutral -> Value

  Show Normal where
    show (MkNormal normalType normalValue) = show normalValue ++ " : " ++ show normalType

  Show Neutral where
    show (NVar x) = x
    show (NApp x y) = "(" ++ show x ++ " "++ show y ++ ")"
    show (NRec ty tgt base step) = "rec[" ++ show ty ++ "] "++ show tgt ++ " " ++ show base ++ " " ++ show step

  Show Value where
    show VZero = "0"
    show (VAdd1 x) = "(" ++ show x ++ " + 1)"
    show (VClosure xs x y) = show x ++ " = " ++ show y ++ "[" ++ show xs ++ "]"
    show (VNeutral x y) = show y ++ " : " ++ show x


mutual

  ||| Evaluation
  eval : Env Value -> Expr -> Either Message Value
  eval x (Var y) = lookupVar x y
  eval x (Lam y z) = Right (VClosure x y z)
  eval x (App y z) = do
    fun <- eval x y
    arg <- eval x z
    doApply fun arg
  eval x Zero = Right VZero
  eval x (Add1 n) = VAdd1 <$> eval x n
  eval x (Rec ty tgt base step) = do
    tgt' <- eval x tgt
    base' <- eval x base
    step' <- eval x step
    doRec ty tgt' base' step'

  doRec : Ty -> Value -> Value -> Value -> Either Message Value
  doRec ty VZero base _ = Right base
  doRec ty (VAdd1 x) base step = do
    f <- doApply step x
    a <- doRec ty x base step
    doApply f a
  doRec ty (VNeutral TNat neut) base step =
    Right $ VNeutral ty (NRec ty neut
                         (MkNormal ty base)
                         (MkNormal (TArr TNat (TArr ty ty)) step))

  doRec ty tgt base step = failure $ "cannot compute primitive recursion [" ++ show ty ++ " " ++ show tgt ++ " " ++ show base ++ " " ++ show step

  ||| Apply a value to its args
  ||| We cannot make neither this nor `eval` function `total` as they can
  ||| diverge
  doApply : Value -> Value -> Either Message Value
  doApply (VClosure env x body) arg = eval (extend env x arg) body
  doApply (VNeutral (TArr dom cod) neut) arg = Right $ VNeutral cod (NApp neut (MkNormal dom arg))
  doApply (VNeutral TNat neut) arg = failure $ "can't apply a Nat value " ++ show arg

  ||| Read back normal form to Expr
  readBackNormal : [Name] -> Normal -> Either Message Expr
  readBackNormal used (MkNormal ty v) = readBack used t v

  ||| Converts a `Value` back into a (syntactic) `Expr` taking care of bound names
  readBack : List Name -> Ty -> Value -> Either Message Expr
  readBack used fun@(VClosure xs x y) = do
    -- freshen variable to ensure it does not capture already bound names
    let x' = freshen used x
    val <- doApply fun (VNeutral TNat (NVar x'))
    exp <- readBack (x' :: used) val
    Right (Lam x' exp)

  readBack _    (VNeutral _ (NVar x)) = Right (Var x)
  readBack used (VNeutral ty (NApp fun arg)) = do
    f <- readBack used (VNeutral ty fun)
    --a <- readBack used arg
    Right (App f ?hole)

normalize : Expr -> Either Message Expr
normalize expr = eval initialEnv expr >>= readBack []

addDefs : Env Value -> Env Expr -> Either Message (Env Value)
addDefs env [] = Right env
addDefs env ((name, expr) :: xs) = do v <- eval env expr
                                      addDefs (extend env name v) xs

program : Env Expr -> Expr -> Either Message Expr
program defs expr = do env <- addDefs initialEnv defs
                       val <- eval env expr
                       readBack (map fst defs) val

||| Example: Church numerals

churchDefs : Env Expr
churchDefs = [ ("zero",
                Lam "f"
                  (Lam "x" (Var "x")))
             , ("add1",
                Lam "n"
                  (Lam "f"
                    (Lam "x"
                      (App (Var "f")
                        (App (App (Var "n")
                                (Var "f"))
                            (Var "x"))))))
             , ("+",
                Lam "j"
                  (Lam "k"
                    (Lam "f"
                      (Lam "x"
                        (App (App (Var "j")
                               (Var "f"))
                          (App (App (Var "k")
                                 (Var "f"))
                            (Var "x")))))))
             ]

toChurch : Nat -> Expr
toChurch Z = Var "zero"
toChurch (S  n) = App (Var "add1") (toChurch n)
