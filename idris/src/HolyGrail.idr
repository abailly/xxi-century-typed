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

-- an impossible value whcih we use to construct 
VOID : _|_
VOID = hd []
where
  hd : List a -> a
  hd (x :: xs) = x
  
notANumber : Answer (QCM question qcmOptions expected) -> Void
notANumber _ = VOID

tooLargeOption : Answer (QCM question qcmOptions expected) -> Void
tooLargeOption _ = VOID 

readAnswer : (s : String) -> (q : Question) -> Dec (Answer q)
readAnswer s (QCM {numOptions} question qcmOptions expected) = 
  case parsePositive s of
    Nothing => No notANumber
    (Just n) => case integerToFin n numOptions of
                     Nothing => No tooLargeOption
                     (Just m) => Yes (AnswerQCM m)
