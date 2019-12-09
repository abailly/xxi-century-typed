module Expr

%access public export

%default covering

||| Identifiers, simply strings at this stage
Name : Type
Name = String

||| Simple-minded way of generating more identifiers
nextName : Name -> Name
nextName x = x ++ "'"

||| Ensures `name` is fresh in given environment

||| Returns the refreshed name
freshen : List Name -> Name -> Name
freshen used x =
  if x `elem` used
  then freshen used (nextName x)
  else x

||| Environments
||| They are polymorphic as a `Name` can be associated to different type of
||| "things"
Env : Type -> Type
Env val = List (Name, val)

initialEnv : Env val
initialEnv = []

-- Error handling
Message : Type
Message = String

failure : String -> Either Message a
failure = Left

||| Lookup a variable name in an environment
lookupVar : Env val -> Name -> Either Message val
lookupVar [] name = failure $ "Variable not found: " ++ name
lookupVar ((n, v) :: env') name =
  if n == name
  then Right v
  else lookupVar env' name

||| Extend the given environment, binding `name` to `val`
extend : Env val -> Name -> val -> Env val
extend env name val = (name, val) :: env

mutual

  ||| Basic typed λ-calculus expressions
  data Expr : Type where
    ||| Variable reference
    |||
    ||| ```
    ||| x
    ||| ```
    Var : Name -> Expr

    ||| λ-abstraction: binds a variable in the context of an expression
    ||| ```
    ||| λ x . <expr>
    ||| ```
    Lam : Name -> Expr -> Expr

    ||| application
    ||| ```
    ||| (x y)
    ||| ```
    App : Expr -> Expr -> Expr

    ||| Builtin natural numbers constructor
    Zero : Expr
    Add1 : Expr -> Expr

    ||| Primitive recursion over a type
    |||
    ||| @tgt the target of the recursion
    ||| @base base value to apply `step`
    ||| @step the function to apply at each step of the recursion
    Rec : Ty -> (tgt : Expr) -> (base : Expr) -> (step : Expr) -> Expr

    ||| Type annotations
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
    show Zero = "Z"
    show (Add1 x) = "(S " ++ show x ++ ")"
    show (Rec x tgt base step) = "rec[" ++ show x ++ "] " ++ show tgt ++ " " ++ show base ++ " " ++ show step
    show (Ann x y) = show x ++ " : " ++ show y

  Show Ty where
    show TNat = "Nat"
    show (TArr dom cod) = show dom ++ " -> " ++ show cod

  Eq Ty where
    TNat == TNat = True
    (TArr x y) == (TArr x' y') = x == x' && y == y'
    _ == _ = False


mutual

  ||| Normal forms are `Ty`ped  `Value`s
  record Normal where
    constructor MkNormal
    normalType : Ty
    normalValue : Value

  ||| Neutral values are either free variables or application of neutral function
  ||| to a `Value`
  ||| They are introduced when reading back lambdas after an evaluation step, to represent
  ||| the fact we don't know the value of some variable
  data Neutral : Type where
    NVar : Name -> Neutral
    NApp : Neutral -> Normal -> Neutral
    NRec : Ty -> Neutral -> Normal -> Normal -> Neutral

  ||| Values are `Expr` closed over the environment in which they are evaluated
  data Value : Type where
    VZero : Value
    VAdd1 : Value -> Value
    VClosure : Env Value -> Name -> Expr -> Value
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
  eval x (Ann e ty) = eval x e

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

  doRec ty tgt base step = failure $ "cannot compute primitive recursion [" ++ show ty ++ " " ++ show tgt ++ " " ++ show base ++ " " ++ show step ++ "]"

  ||| Apply a value to its args
  ||| We cannot make neither this nor `eval` function `total` as they can
  ||| diverge
  doApply : Value -> Value -> Either Message Value
  doApply (VClosure env x body) arg = eval (extend env x arg) body
  doApply (VNeutral (TArr dom cod) neut) arg = Right $ VNeutral cod (NApp neut (MkNormal dom arg))
  doApply val arg = failure $ "can't apply value " ++ show val ++ " to argument " ++ show arg

  ||| Read back normal form to Expr
  readBackNormal : List Name -> Normal -> Either Message Expr
  readBackNormal used (MkNormal ty v) = readBack used ty v

  ||| Converts a `Value` back into a (syntactic) `Expr` taking care of bound names
  readBack : List Name -> Ty -> Value -> Either Message Expr
  readBack used TNat VZero = pure Zero
  readBack used TNat (VAdd1 x) = Add1 <$> readBack used TNat x
  readBack used (TArr dom cod) fun@(VClosure xs arg y) =
    readBackLambda used dom cod fun arg
  readBack used (TArr dom cod) fun =
    readBackLambda used dom cod fun "x"
  readBack used t1 (VNeutral t2 neu) =
    if t1 == t2
      then readBackNeutral used neu
      else failure ("Mismatched types when reading back neutral: expected " ++ show t1 ++ ", found " ++ show t2)
  readBack used ty val = failure ("Cannot read back expression from type " ++ show ty ++ " and value " ++ show val)

  readBackLambda : List Name -> Ty -> Ty -> Value -> Name -> Either Message Expr
  readBackLambda  used dom cod fun arg =
    let x = freshen used arg
        xval = VNeutral dom (NVar x)
    in Lam x <$> (doApply fun xval >>= readBack used cod)

  readBackNeutral : List Name -> Neutral -> Either Message Expr
  readBackNeutral used (NVar x) = pure $ Var x
  readBackNeutral used (NApp x y) =
    App <$> readBackNeutral used x <*> readBackNormal used y
  readBackNeutral used (NRec ty neu base step) =
    Rec ty <$>
    readBackNeutral used neu <*>
    readBackNormal used base <*>
    readBackNormal used step

Defs : Type
Defs = Env Normal

noDefs : Defs
noDefs = initialEnv


-- program : Env Expr -> Expr -> Either Message Expr
-- program defs expr = do env <- addDefs initialEnv defs
--                        val <- eval env expr
--                        readBack (map fst defs) val

-- ||| Example: Church numerals

-- churchDefs : Env Expr
-- churchDefs = [ ("zero",
--                 Lam "f"
--                   (Lam "x" (Var "x")))
--              , ("add1",
--                 Lam "n"
--                   (Lam "f"
--                     (Lam "x"
--                       (App (Var "f")
--                         (App (App (Var "n")
--                                 (Var "f"))
--                             (Var "x"))))))
--              , ("+",
--                 Lam "j"
--                   (Lam "k"
--                     (Lam "f"
--                       (Lam "x"
--                         (App (App (Var "j")
--                                (Var "f"))
--                           (App (App (Var "k")
--                                  (Var "f"))
--                             (Var "x")))))))
--              ]

-- toChurch : Nat -> Expr
-- toChurch Z = Var "zero"
-- toChurch (S  n) = App (Var "add1") (toChurch n)
