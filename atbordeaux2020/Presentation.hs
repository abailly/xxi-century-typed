{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentation where
{-

           TDD & TDD sont dans un bateau

           Une expérience d'Arnaud Bailly

                        pour

               Agile Tour Bordeaux 2020
                   29 octobre 2020

-}

import Test.Hspec
import Data.String
import Test.QuickCheck
import Data.Char (toUpper, isUpper, isLower, toLower)

-- * Questions & Casse

-- Une question d'un Quizz
--
-- >>> let obvious = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"
data Question = Q {question :: String, reponseAttendue :: SansCasse}

-- Test Driven Development pour verifier la réponse
verifieSpec :: Spec
verifieSpec = describe "Verifie la réponse" $ do
  let question1 = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"

  it "retourne True si la réponse donnée est égale à la réponse attendue" $ do
    verifieLaRéponse "blanc" question1 `shouldBe` True

  it "retourne False si la réponse donnée n'est pas égale à la réponse attendue" $ do
    verifieLaRéponse "noir" question1 `shouldBe` False


verifieLaRéponse :: SansCasse -> Question -> Bool
verifieLaRéponse proposition Q{reponseAttendue} =
  proposition == reponseAttendue

-- un type représentant les chaînes de caractères insensibles à la casse
newtype SansCasse = SansCasse { sansCasse :: String }
  deriving (Show)

instance IsString SansCasse where
  fromString = SansCasse

instance Eq SansCasse where
  SansCasse sc1 == SansCasse sc2 =
    fmap toLower sc1 == fmap toLower sc2

-- une instance de SansCasse arbitraire est construite à partir
-- d'une instance de chaine ASCII arbitraire (pour des raisons de simplicité)
instance Arbitrary SansCasse where
  arbitrary = SansCasse . getASCIIString <$> arbitrary

-- exprimer la propriété que 2 chaines `SansCasse` sont égales si
-- leurs représentations en majuscules sont égales
egaliteSansCasse :: SansCasse -> Property
egaliteSansCasse sc@(SansCasse base) =
  collect (length base) $ SansCasse (permuteCasse base) == sc

permuteCasse :: String -> String
permuteCasse [] = []
permuteCasse (c:cs)
  | isUpper c =  toLower c : permuteCasse cs
  | isLower c =  toUpper c : permuteCasse cs
  | otherwise = c : permuteCasse cs


-- * Numéro de Sécurité Sociale

-- Un type naïf pour les numéros de sécurité sociale
-- voir https://www.ameli.fr/loire-atlantique/assure/droits-demarches/principes/numero-securite-sociale
newtype NIR1 = NIR1 String
  deriving (Eq, Show)

valideNIR :: NIR1 -> Bool
valideNIR (NIR1 _nir) = undefined
