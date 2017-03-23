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

data Input : Type -> Type where
  Number : (number : Nat) -> Input Nat
  Text : (text : String) -> Input String
  
-- data Answer : (q : Question oracle) -> Type where
--   AnswerQCM : (option : Input Nat) -> {auto prf : LTE (number option) numOptions } -> Answer (QCM {numOptions = n } q opts exp)

record Quizz where
  constructor MkQuizz
  answered : Nat
  remaining : Nat
  current : Nat
  questions : Vect (answered + remaining) (Question oracle)

-- total 
-- isCorrectAnswer : (q : Question oracle) -> Answer _ q -> Bool
-- isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM option) = 
--                 case natToFin option numOptions of
--                   Nothing => False
--                   (Just x) => x == expected
  

readInput : (s : String) -> (a ** Input a)
readInput s = 
   case parsePositive s of
     Nothing  => (_ ** Text s) 
     (Just n) => (_ ** Number (fromInteger n))

-- readAnswer : (q : Question oracle) -> String -> Dec (Answer a q)
-- readAnswer (QCM {numOptions} question qcmOptions expected) x = 
--   case parsePositive x of
--     Nothing => No (invalidString x)
--     (Just n) => case isLTE (fromInteger n) numOptions of
--                      (Yes prf) => Yes ?okbranch
--                      (No contra) => ?hole_2
