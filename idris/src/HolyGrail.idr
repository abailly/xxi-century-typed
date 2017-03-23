module HolyGrail

import Data.String
import Data.Vect
import Data.Fin

data Oracle : Type where
  QCMOption : Fin n -> Oracle
  
data Question : (oracle : Oracle) -> Type where
  QCM : {numOptions : Nat}
      -> (question : String) 
      -> (qcmOptions : Vect numOptions String)
      -> (expected : Fin numOptions)
      -> Question (QCMOption expected)

data Answer : (q : Question oracle) -> Type where
   AnswerQCM : (option : Fin n) -> Answer (QCM {numOptions = n } q opts exp)

total 
isCorrectAnswer : (q : Question oracle) -> Answer q -> Bool
isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM option) = option == expected
