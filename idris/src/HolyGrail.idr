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
  AnswerQCM : (option : Nat) -> {auto prf : LTE option numOptions } -> Answer (QCM {numOptions = n } q opts exp)

record Quizz where
  constructor MkQuizz
  answered : Nat
  remaining : Nat
  current : Nat
  questions : Vect (answered + remaining) (Question oracle)

total 
isCorrectAnswer : (q : Question oracle) -> Answer q -> Bool
isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM option) = 
                case natToFin option numOptions of
                  Nothing => False
                  (Just x) => x == expected


readAnswer : (q : Question oracle) -> String -> Dec (Answer q)
readAnswer (QCM {numOptions} question qcmOptions expected) x = 
  case parsePositive x of
    Nothing => No ?invalidString
    (Just n) => case isLTE (fromInteger n) numOptions of
                     (Yes prf) => Yes (AnswerQCM (fromInteger n))
                     (No contra) => ?hole_2
