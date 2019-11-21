module Expr

%default covering

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

||| Basic (untyped) λ-calculus expressions
data Expr : Type where
  Var : Name -> Expr
  Lam : Name -> Expr -> Expr
  App : Expr -> Expr -> Expr

||| Values are `Expr` closed over the environment in which they are evaluated
data Value : Type where
  Closure : (Env Value) -> Name -> Expr -> Value

mutual

  ||| Evaluation
  eval : Env Value -> Expr -> Either Message Value
  eval x (Var y) = lookupVar x y
  eval x (Lam y z) = Right (Closure x y z)
  eval x (App y z) = do
    fun <- eval x y
    arg <- eval x z
    doApply fun arg

  ||| Apply a value to its args
  ||| We cannot make neither this nor `eval` function `total` as they can
  ||| diverge
  doApply : Value -> Value -> Either Message Value
  doApply (Closure env x body) arg = eval (extend env x arg) body


addDefs : Env Value -> Env Expr -> Either Message (Env Value)
addDefs env [] = Right env
addDefs env ((name, expr) :: xs) = do v <- eval env expr
                                      addDefs (extend env name v) xs

program : Env Expr -> Expr -> Either Message Value
program defs expr = do env <- addDefs initialEnv defs
                       eval env expr

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
