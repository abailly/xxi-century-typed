module Minilang.PrettySpec where

import           Data.Monoid     ((<>))
import qualified Data.Text       as Text
import           Minilang.Parser
import           Minilang.Pretty
import           Test.Hspec
import           Test.QuickCheck as QC


spec :: Spec
spec = parallel $ describe "Pretty-printing Expressions" $ do

  it "true is always true" $ True `shouldBe` True

  it "is inverse to parsing" $ property $ prop_parsingIsInvertToPrettyPrinter

  it "has parsing as inverse" $ property $ prop_prettyPrintingIsInverseToParsing

  it "pretty prints simple expressions" $ do
    show (pretty (parseProgram False "Π abc : U . abc") )
      `shouldBe` "Π abc : U . abc"
    show (pretty (parseProgram False "λ abc . π1.(abc,feg)") )
      `shouldBe` "λ abc . π1.(abc, feg)"
    show (pretty (parseProgram False "λ (abc, (x,y)) . π1.($true, $false)") )
      `shouldBe` "λ (abc, (x, y)) . π1.($true, $false)"

  it "pretty prints declarations" $ do
    show (pretty (parseProgram False "let id : Π A : U . Π _ : A . A = λ A . λ x . x ;\n()") )
      `shouldBe` "let id : Π A : U . A → A = λ A . λ x . x ;\n()"
    show (pretty (parseProgram False "let rec Nat : U = Sum (zero | succ Nat) ;\nlet id : Π A : U . Π _ : A . A = λ A . λ x . x; ()"))
      `shouldBe` "let rec Nat : U = Sum(zero| succ Nat) ;\nlet id : Π A : U . A → A = λ A . λ x . x ;\n()"
    show (pretty (parseProgram False "let rec natrec : Π C : Nat → U . C $zero → (Π n : Nat.C n → C ($succ n)) → Π n : Nat . C n = λ C . λ a . λ g . case (zero → a | succ n1 → (g n1) (natrec C a g n1)); ()"))
      `shouldBe` "let rec natrec : Π C : Nat → U . (C $zero) → Π n : Nat . (C n) → (C ($succ n)) → Π n : Nat . (C n) = λ C . λ a . λ g . case(zero → λ _ . a| succ → λ n1 . ((g n1) ((((natrec C) a) g) n1))) ;\n()"


newtype TestAST = T { unAst :: AST }
  deriving (Eq, Show)

arbitraryAst :: Gen AST
arbitraryAst = unAst <$> arbitrary

leafAST :: [ (Int, Gen AST) ]
leafAST = [ (1, pure One)
          , (1, pure Unit)
          , (1, I <$> arbitrary)
          , (1, D <$> arbitrary)
          ]

genAST :: Int ->  Gen AST
genAST 0 = frequency leafAST
genAST n =
  frequency $ leafAST <> [ (3, Ap <$> genAST (n-1) <*> genAST (n-1)) ]

instance Arbitrary TestAST where
  arbitrary = do
    h <- QC.choose (0, 4)
    T <$> genAST h

prop_parsingIsInvertToPrettyPrinter :: TestAST -> Property
prop_parsingIsInvertToPrettyPrinter (T ast) =
  let pp = show (pretty ast)
      parsed = parseProgram False (Text.pack pp)
      msg = "from : " <> show ast <> ", pretty: " <> pp <> ", back to: " <> show parsed
  in collect (height ast) $ counterexample msg $ parsed == ast

prop_prettyPrintingIsInverseToParsing :: TestAST -> Property
prop_prettyPrintingIsInverseToParsing (T ast) =
  let pp = show (pretty ast)
      parsed = parseProgram False (Text.pack pp)
      msg = "from : " <> show ast <> ", pretty: " <> pp <> ", pretty parsed is: " <> show (pretty parsed)
  in counterexample msg $ pp == show (pretty parsed)
