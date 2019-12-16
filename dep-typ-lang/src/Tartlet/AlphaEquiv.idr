||| Compute alpha-equivalence of `Expr`
||| This deserves its own module as α-equivalence needs to compare
||| all possible expressions.
||| see Section 6.1
module Tartlet.AlphaEquiv

import Expr

%default covering

||| Environments
||| They are polymorphic as a `Name` can be associated to different type of
||| "things"
Env : Type -> Type
Env val = List (Name, val)

initialEnv : Env val
initialEnv = []


alphaEquivHelper : Integer -> Env Integer -> Expr -> Env Integer -> Expr -> Bool
alphaEquivHelper l ns1 (Var x) ns2 (Var y) =
  -- two variables are α-equivalent if either...
  case (lookup x ns1, lookup y ns2) of
    -- ...  they are unbound and the same variable
    (Nothing, Nothing) => x == y
    -- ... they are bound at the same level
    (Just i, Just j) => i == j
    _ => False

alphaEquivHelper l ns1 (Pi x t1 b1) ns2 (Pi y t2 b2) =
  -- first check the α-equiv of type annotations ...
  alphaEquivHelper l     ns1 t1 ns2 t2 &&
  -- ... then check the bodys in an extended context
  alphaEquivHelper (l+1) ((x,l) :: ns1) b1 ((y,l) :: ns2) b2

alphaEquivHelper l ns1 (Sigma x t1 b1) ns2 (Sigma y t2 b2) =
  -- works like Pi
  alphaEquivHelper l     ns1 t1 ns2 t2 &&
  alphaEquivHelper (l+1) ((x,l) :: ns1) b1 ((y,l) :: ns2) b2

alphaEquivHelper l ns1 (Lam x b1) ns2 (Lam y b2) =
  -- extend context with bound variables and recursively check α-equiv of bodies
  alphaEquivHelper (l+1) ((x,l) :: ns1) b1 ((y,l) :: ns2) b2

alphaEquivHelper l ns1 (App f x) ns2 (App g y) =
  -- Applications are equivalent if both sides are
  alphaEquivHelper l ns1 f ns2 g &&
  alphaEquivHelper l ns1 x ns2 y

alphaEquivHelper l ns1 (Pair x x') ns2 (Pair y y') =
  -- Same as App...
  alphaEquivHelper l ns1 x ns2 y &&
  alphaEquivHelper l ns1 y ns2 y'

alphaEquivHelper l ns1 (P1 x) ns2 (P1 y) =
  -- P1 and P2 are identical
  alphaEquivHelper l ns1 x ns2 y
alphaEquivHelper l ns1 (P2 x) ns2 (P2 y) =
  alphaEquivHelper l ns1 x ns2 y

-- constants are always equivalent
alphaEquivHelper _ _ Z _ Z = True
alphaEquivHelper _ _ Nat _ Nat = True
alphaEquivHelper _ _ Same _ Same = True
alphaEquivHelper _ _ Trivial _ Trivial = True
alphaEquivHelper _ _ Sole _ Sole = True
alphaEquivHelper _ _ Absurd _ Absurd = True
alphaEquivHelper _ _ Atom _ Atom = True
alphaEquivHelper _ _ U _ U = True

alphaEquivHelper l ns1 (S x) ns2 (S y) =
  alphaEquivHelper l ns1 x ns2 y

alphaEquivHelper l ns1 (Tick s1) ns2 (Tick s2) = s1 == s2

alphaEquivHelper l ns1 (IndNat tgt1 mot1 base1 step1) ns2 (IndNat tgt2 mot2 base2 step2) =
  alphaEquivHelper l ns1 tgt1 ns2 tgt2 &&
  alphaEquivHelper l ns1 mot1 ns2 mot2 &&
  alphaEquivHelper l ns1 base1 ns2 base2 &&
  alphaEquivHelper l ns1 step1 ns2 step2

alphaEquivHelper l ns1 (Replace tgt1 mot1 base1) ns2 (Replace tgt2 mot2 base2) =
  alphaEquivHelper l ns1 tgt1 ns2 tgt2 &&
  alphaEquivHelper l ns1 mot1 ns2 mot2 &&
  alphaEquivHelper l ns1 base1 ns2 base2

alphaEquivHelper l ns1 (Equal ty1 from1 to1) ns2 (Equal ty2 from2 to2) =
  alphaEquivHelper l ns1 ty1 ns2 ty2 &&
  alphaEquivHelper l ns1 from1 ns2 from2 &&
  alphaEquivHelper l ns1 to1 ns2 to2

alphaEquivHelper l ns1 (IndAbsurd tgt1 mot1 ) ns2 (IndAbsurd tgt2 mot2 ) =
  alphaEquivHelper l ns1 tgt1 ns2 tgt2 &&
  alphaEquivHelper l ns1 mot1 ns2 mot2

alphaEquivHelper l ns1 (The Absurd _) ns2 (The Absurd _) = True

alphaEquivHelper l ns1 (The ty1 e1) ns2 (The ty2 e2) =
  alphaEquivHelper l ns1 ty1 ns2 ty2 &&
  alphaEquivHelper l ns1 e1 ns2 e2

alphaEquivHelper _ _ _ _ _ = False

alphaEquiv : Expr -> Expr -> Bool
alphaEquiv e e' = alphaEquivHelper 0 [] e [] e'
