module Minilang.NormalizeSpec where

import Minilang.Env
import Minilang.Eval hiding (NCase)
import qualified Minilang.Eval as Eval
import Minilang.Normalize
import Minilang.Parser
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Positive (..))

spec :: Spec
spec = parallel $
    describe "Normalizer" $ do
        it "normalizes constants to constants" $ do
            normalize 0 EUnit `shouldBe` NUnit
            normalize 0 (EI 12) `shouldBe` NI 12
            normalize 0 (ED 12) `shouldBe` ND 12
            normalize 0 (ES "foo") `shouldBe` NS "foo"

        prop "normalizes universes" $ \(Universe eu@(EU n)) -> normalize 0 eu == NU n

        it "normalizes constructor" $ do
            normalize 0 (ECtor "foo" (Just EUnit))
                `shouldBe` NCtor "foo" (Just NUnit)
            normalize 0 (ECtor "foo" Nothing)
                `shouldBe` NCtor "foo" Nothing

        it "normalizes pairs" $ do
            normalize 0 (EPair EUnit (EI 12))
                `shouldBe` NPair NUnit (NI 12)

        it "normalizes abstraction" $ do
            normalize 0 (EAbs (Cl (B "x") (Var "x") EmptyEnv))
                `shouldBe` NAbs (NVar 0) (NNeut (NNV (NVar 0)))

        it "normalizes application" $ do
            normalize 1 (NAp (NV $ NVar 0) (EI 12))
                `shouldBe` NNAp (NNV $ NVar 0) (NI 12)

        it "normalizes Projections" $ do
            normalize 1 (NP1 (NV $ NVar 0))
                `shouldBe` NNPi1 (NNV $ NVar 0)
            normalize 1 (NP2 (NV $ NVar 0))
                `shouldBe` NNPi2 (NNV $ NVar 0)

        it "normalizes case neutral application with env" $ do
            let extended = ExtendPat emptyEnv (B "x") (ENeut $ NV $ NVar 1)

            normalize 0 (Eval.NCase (CaseClos ([Clause "A" (Abs (B "x") (Var "x"))], extended)) (NV $ NVar 1))
                `shouldBe` NNCase
                    ( [Clause "A" (Abs (B "x") (Var "x"))]
                    , ExtendPat EmptyEnv (B "x") (NNeut (NNV (NVar 1)))
                    )
                    (NNV (NVar 1))

        it "normalizes env with declaration" $ do
            normalize 0 (ExtendDecl EmptyEnv (Decl (B "x") (U 0) Unit) :: Env)
                `shouldBe` (ExtendDecl EmptyEnv (Decl (B "x") (U 0) Unit) :: NEnv)

        it "normalizes Pi expression" $ do
            let extended = ExtendPat emptyEnv (B "bar") (EI 12)
            normalize 0 (EPi (EU 0) (Cl (B "foo") (Var "bar") extended))
                `shouldBe` NPi (NVar 0) (NU 0) (NI 12)

        it "normalizes Sigma expression" $ do
            let extended = ExtendPat emptyEnv (B "bar") (EI 12)
            normalize 0 (ESig (EU 0) (Cl (B "foo") (Var "bar") extended))
                `shouldBe` NSig (NVar 0) (NU 0) (NI 12)

        it "normalizes Sum definitions" $ do
            normalize
                0
                ( ESum
                    ( SumClos
                        ( [Choice "true" (Just Unit), Choice "false" Nothing]
                        , emptyEnv
                        )
                    )
                )
                `shouldBe` NSum ([Choice "true" (Just Unit), Choice "false" Nothing], EmptyEnv)

        it "normalizes Case expressions" $ do
            normalize
                0
                ( ECase
                    ( CaseClos
                        ( [Clause "true" Unit, Clause "false" Unit]
                        , emptyEnv
                        )
                    )
                )
                `shouldBe` NCase ([Clause "true" Unit, Clause "false" Unit], EmptyEnv)

        describe "normal forms equality" $ do
            it "compares expression for Sum" $ do
                let extended = ExtendPat emptyEnv (B "bar") (EI 12)
                    n1 =
                        normalize
                            0
                            ( ESum
                                ( SumClos
                                    ( [Choice "true" (Just Unit), Choice "false" Nothing]
                                    , emptyEnv
                                    )
                                )
                            )
                    n2 =
                        normalize
                            0
                            ( ESum
                                ( SumClos
                                    ( [Choice "true" (Just Unit), Choice "false" Nothing]
                                    , extended
                                    )
                                )
                            )
                same n1 n2 `shouldBe` True

newtype Universe = Universe Value
    deriving (Show, Eq)

instance Arbitrary Universe where
    arbitrary = Universe . EU . fromInteger . getPositive <$> arbitrary
