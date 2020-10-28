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

  it "retourne False si la réponse donnée est vide" $ do
    verifieLaRéponse "" question1 `shouldBe` False


verifieLaRéponse :: String -> Question -> Bool
verifieLaRéponse (p:proposition) Q{reponseAttendue} =
  toLower p : proposition == reponseAttendue
