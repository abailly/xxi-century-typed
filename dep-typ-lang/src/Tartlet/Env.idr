module Tartlet.Env

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

ctxNames : Env a -> List Name
ctxNames = map fst
