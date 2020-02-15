{-# OPTIONS_GHC -fno-warn-orphans #-}
module Minilang.Pretty
  (pretty)
where

import           Data.Text.Prettyprint.Doc
import           Minilang.Env
import           Minilang.Eval             as Eval
import           Minilang.Normalize        as Norm
import           Minilang.Parser
import           Minilang.Primitives


-- * Pretty-Printing

-- ** Expressions

instance Pretty AST where
  pretty U                 = "U"
  pretty One               = "[]"
  pretty Unit              = "()"
  pretty (I n)             = pretty n
  pretty (D d)             = pretty d
  pretty (S s)             = dquotes $ pretty s
  pretty (Abs p e)         = "λ" <+> pretty p <+> "." <+> pretty e
  pretty (Ctor n Nothing)  = "$" <> pretty n
  pretty (Ctor n (Just e)) = parens ("$" <> pretty n <+> pretty e)
  pretty (Pi Wildcard t e) = pretty t <+> "→" <+> pretty e
  pretty (Pi p t e)        = "Π" <+> pretty p <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty (Sigma p t e)     = "Σ" <+> pretty p <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty (Pair e1 e2)      = parens (pretty e1 <> "," <+> pretty e2)
  pretty (Sum cs)          = "Sum" <> parens (hsep $ punctuate "|" (fmap prettySum cs))
  pretty (Case cs)         = "case" <> parens (hsep $ punctuate "|" (fmap prettyFun cs))
  pretty (Var n)           = pretty n
  pretty (Ap e1 e2)        = parens $ pretty e1 <+> pretty e2
  pretty (P1 e)            = "π1." <> pretty e
  pretty (P2 e)            = "π2." <> pretty e
  pretty (Def d e)         = "def" <+> pretty d <+> ";" <> hardline <> pretty e
  pretty (Err t)           = pretty (show t)

instance Pretty Decl where
  pretty (Decl p t e)  = pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e
  pretty (RDecl p t e) = "rec" <+> pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e

instance Pretty Binding where
  pretty (Pat l r) = parens (pretty l <> "," <+> pretty r)
  pretty (B n)     = pretty n
  pretty (C e)     = pretty e
  pretty Wildcard  = "_"


prettySum :: Choice -> Doc ann
prettySum (Choice t Nothing)  = pretty t
prettySum (Choice t (Just e)) = pretty t <+> pretty e


prettyFun :: Clause -> Doc ann
prettyFun (Clause t e)   = pretty t <+> "→" <+> pretty e

-- ** Values

instance Pretty Value where
  pretty EU              = "U"
  pretty EUnit           = "()"
  pretty EOne            = "[]"
  pretty (EI n)          = pretty n
  pretty (ED d)          = pretty d
  pretty (ES s)          = dquotes $ pretty s
  pretty (EPrim p)       = pretty p
  pretty (ENeut nt)      = pretty nt
  pretty (EAbs f)        = pretty f
  pretty (ECtor n v)     = "$" <> pretty n <+> pretty v
  pretty (EPi v f)       = "Π" <+> pretty v <+> "." <+> pretty f
  pretty (ESig v f)      = "Σ" <+> pretty v <+> "." <+> pretty f
  pretty (EPair l r)     = parens (pretty l  <> "," <+> pretty r)
  pretty (ESum (SumClos (cs, _)))  = "Sum" <> parens (hsep (punctuate "|" (fmap prettySum cs)))
  pretty (ECase (CaseClos (cs, _))) = "case" <> parens (hsep (punctuate "|" (fmap prettyFun cs)))

instance Pretty FunClos where
  pretty (Cl b e ρ)    = angles ("λ" <+> pretty b <+> "." <+> pretty e <+> "," <+> pretty ρ)
  pretty (ClComp f c)  = pretty f <+> "*" <+> pretty c
  pretty (ClComp0 f c) = pretty f <+> "*" <+> pretty c

instance Pretty  NVar where
  pretty (NVar n) = "#" <> pretty n

instance  Pretty Neutral where
  pretty (NV x)            = pretty x
  pretty (NP1 nt)          = "π1." <> pretty nt
  pretty (NP2 nt)          = "π2." <> pretty nt
  pretty (Eval.NCase (CaseClos (cs,_)) nt) = angles ("case" <> parens (hsep $ punctuate "|" (fmap prettyFun cs))  <+> pretty nt)
  pretty (NAp nt v)        = parens (pretty nt <+> pretty v)

-- ** Normal Forms

instance Pretty Normal where
  pretty (NAbs v nf)   = "λ" <+> pretty v <+> "." <+> pretty nf
  pretty (NPi v t e)   = "Π" <+> pretty v <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty (NSig v t e)  = "Σ" <+> pretty v <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty NU            = "U"
  pretty (NNeut nt)    = pretty nt
  pretty (NPair l r)   = parens (pretty l <> "," <+> pretty r)
  pretty NUnit         = "()"
  pretty NOne          = "[]"
  pretty (NI i)        = pretty i
  pretty (ND d)        = pretty d
  pretty (NS s)        = dquotes $ pretty s
  pretty (NPrim p)     = pretty p
  pretty (NCtor n Nothing)   = "$" <> pretty n
  pretty (NCtor n (Just nf)) = parens ("$" <> pretty n <+> pretty nf)
  pretty (NSum (cs,_)) = "Sum" <> parens (hsep (punctuate "|" (fmap prettySum cs)))
  pretty (Norm.NCase (cs,_)) = "case" <> parens (hsep (punctuate "|" (fmap prettyFun cs)))


instance  Pretty NNeutral where
  pretty (NNV x)            = pretty x
  pretty (NNPi1 nt)          = "π1." <> pretty nt
  pretty (NNPi2 nt)          = "π2." <> pretty nt
  pretty (NNCase (cs,ρ) nt) = angles ("case" <> parens (hsep $ punctuate "|" (fmap prettyFun cs))  <+> pretty nt <> "," <+> pretty ρ)
  pretty (NNAp nt v)        = parens (pretty nt <+> pretty v)


-- ** Environment

instance (Pretty value) => Pretty (Env' value) where
  pretty env = braces (pretty' env)
    where
      pretty' EmptyEnv                     = "∅"
      pretty' (ExtendPat ρ b v)            = pretty b  <> " ↦ " <> pretty v <> ", " <> pretty' ρ
      pretty' (ExtendDecl ρ (Decl b t m))  = pretty b  <> " : " <> pretty t <> " ↦ " <> pretty m <> ", " <> pretty' ρ
      pretty' (ExtendDecl ρ (RDecl b t m)) = pretty b  <> " : " <> pretty t <> " ↦ " <> pretty m <> ", " <> pretty' ρ


-- ** Context

instance Pretty Context where
  pretty env = braces (pretty' env)
    where
      pretty' EmptyContext    = "∅"
      pretty' (Context γ n v) = pretty n <> " ↦ " <> pretty v <> ", " <> pretty' γ

-- ** Primitives

instance Pretty PrimType where
  pretty PrimInt    = "#Int"
  pretty PrimDouble = "#Double"
  pretty PrimString = "#String"
