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
data Question = Q { question :: Text, reponse :: Text }
