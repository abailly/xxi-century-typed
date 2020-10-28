{-# LANGUAGE NamedFieldPuns #-}

module Presentation where
{-

           TDD & TDD sont dans un bateau

           Une expérience d'Arnaud Bailly

                        pour

               Agile Tour Bordeaux 2020
                   29 octobre 2020

-}

import Test.Hspec
import Test.QuickCheck
import Data.Char (toLower)

-- Une question d'un Quizz
--
-- >>> let obvious = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"
data Question = Q {question :: String, reponseAttendue :: String}

-- Test Driven Development pour verifier la réponse
verifieSpec :: Spec
verifieSpec = describe "Verifie la réponse" $ do
  let question1 = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"

  it "retourne True si la réponse donnée est égale à la réponse attendue" $ do
    verifieLaRéponse "blanc" question1 `shouldBe` True

  it "retourne False si la réponse donnée n'est pas égale à la réponse attendue" $ do
    verifieLaRéponse "noir" question1 `shouldBe` False

  it "retourne True si la réponse donnée est celle attendue à la casse près" $ do
    verifieLaRéponse "Blanc" question1 `shouldBe` True
    verifieLaRéponse "bLanc" question1 `shouldBe` True

    let question2 = Q "Qui a 'inventé' TDD ?" "Kent Beck"

    verifieLaRéponse "Kent Beck" question2 `shouldBe` True

verifieLaRéponse :: String -> Question -> Bool
verifieLaRéponse proposition Q{reponseAttendue} =
  fmap toLower proposition == fmap toLower reponseAttendue

-- un type représentant les chaînes de caractères insensibles à la casse
newtype SansCasse = SansCasse { sansCasse :: String }
  deriving (Show)

-- exprimer la propriété que 2 chaines `SansCasse` sont égales si
-- leurs représentations en majuscules sont égales
egaliteSansCasse :: SansCasse -> Property
egaliteSansCasse sc@(SansCasse base) =
  collect (length base) $ SansCasse (_permuteCasse base) == sc
