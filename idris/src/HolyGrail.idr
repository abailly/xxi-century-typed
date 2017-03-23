module HolyGrail

import Data.String
import Data.Vect
import Data.Fin

data Question : Type where
  QCM : {numOptions : Nat}
      -> (question : String) 
      -> (qcmOptions : Vect numOptions String)
      -> (expected : Fin numOptions)
      -> Question

data Answer : (q : Question) -> Type where
   AnswerQCM : (option : Fin n) -> Answer (QCM {numOptions = n } q opts exp)

total 
isCorrectAnswer : (q : Question ) -> Answer q -> Bool
isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM option) = option == expected
