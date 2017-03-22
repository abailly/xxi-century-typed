module HolyGrail

import Data.Vect
import Data.Fin

data Oracle : Type where
  QCMOption : Fin n -> Oracle
  
data Question : (oracle : Oracle) -> Type where
  QCM : (numOptions : Nat) 
      -> (question : String) 
      -> (qcmOptions : Vect numOptions String)
      -> (expected : Fin numOptions)
      -> (response : Maybe (Fin numOptions))
      -> Question (QCMOption expected)

record Quizz where
  constructor MkQuizz
  answered : Nat
  remaining : Nat
  current : Nat
  questions : Vect (answered + remaining) (Question oracle)

total 
isCorrectAnswer : Question oracle -> Bool
isCorrectAnswer (QCM numOptions question qcmOptions expected Nothing) = False
isCorrectAnswer (QCM numOptions question qcmOptions expected (Just x)) = 
  x == expected
