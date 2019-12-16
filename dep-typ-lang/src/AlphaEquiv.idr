||| Compute alpha-equivalence of `Expr`
||| This deserves its own module as α-equivalence needs to compare
||| all possible expressions.
||| see Section 6.1
module AlphaEquiv

import Tartlet.Expr

alphaEquivHelper : Integer ->

alphaEquiv : Expr -> Expr -> Bool
alphaEquiv e e' = alphaEquivHelper 0 [] e [] e'
