{-# OPTIONS_GHC -fno-warn-orphans #-}
module Minilang.Pretty
  (pretty)
where

import           Data.Text.Prettyprint.Doc
import           Minilang.Parser


instance Pretty AST where
  pretty U             = "U"
  pretty One           = "[]"
  pretty Unit          = "()"
  pretty (I n)         = pretty n
  pretty (D d)         = pretty d
  pretty (Abs p e)     = "λ" <+> pretty p <+> "." <+> pretty e
  pretty (Ctor n Unit)    = "$" <> pretty n
  pretty (Ctor n e)    = "$" <> pretty n <+> pretty e
  pretty (Pi p t e)    = "Π" <+> pretty p <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty (Sigma p t e) = "Σ" <+> pretty p <+> ":" <+> pretty t <+> "." <+> pretty e
  pretty (Pair e1 e2)  = parens (pretty e1 <> "," <+> pretty e2)
  pretty (Sum cs)      = "Sum" <> parens (hsep $ punctuate "|" (fmap prettySum cs))
  pretty (Case _cs)    = undefined
  pretty (Var n)       = pretty n
  pretty (Ap e1 e2)    = pretty e1 <+> pretty e2
  pretty (P1 e)        = "π1." <> pretty e
  pretty (P2 e)        = "π2." <> pretty e
  pretty (Def d e)     = pretty d <+> ";" <> hardline <> pretty e
  pretty (Err _)       = undefined

instance Pretty Decl where
  pretty (Decl p t e)  = pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e
  pretty (RDecl p t e) = "rec" <+> pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e

instance Pretty Binding where
  pretty (Pat l r) = parens (pretty l <> "," <+> pretty r)
  pretty (B n)     = pretty n
  pretty (C e)     = pretty e
  pretty Wildcard  = "_"


prettySum :: Choice -> Doc ann
prettySum (Choice t One) = pretty t
prettySum (Choice t e)   = pretty t <+> pretty e
