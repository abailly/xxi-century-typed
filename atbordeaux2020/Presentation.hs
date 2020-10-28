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

-- Une question d'un Quizz
--
-- >>> let obvious = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"
data Question = Q {question :: String, reponseAttendue :: String}

-- Test Driven Development pour verifier la réponse
verifieSpec :: Spec
verifieSpec = describe "Verifie la réponse" $ do
  it "retourne True si la réponse donnée est égale à la réponse attendue" $ do
    let q = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"

    verifieLaRéponse "blanc" q `shouldBe` True

  it "retourne False si la réponse donnée n'est pas égale à la réponse attendue" $ do
    let q = Q "Quel est la couleur du cheval blanc d'Henri IV?" "blanc"

    verifieLaRéponse "noir" q `shouldBe` False


verifieLaRéponse :: String -> Question -> Bool
verifieLaRéponse proposition Q{reponseAttendue} = proposition == reponseAttendue
