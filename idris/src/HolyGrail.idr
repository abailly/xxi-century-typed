module HolyGrail

import Data.String 
import Data.Vect
import Data.Fin

interface Displayable d where
  total display : d -> String
  
data Question : Type where
  Open : (question : String) 
      -> (expected : String)
      -> Question
  QCM : {numOptions : Nat}
      -> (question : String) 
      -> (qcmOptions : Vect numOptions String)
      -> (expected : Fin numOptions)
      -> Question
  Grade : (question : String)
       -> (bounds : (Nat, Nat))
       -> (expected : Nat)
       -> Question

indexed : Nat ->  Vect n a -> Vect n (Nat, a)
indexed k [] = []
indexed k (x :: xs) = 
  (k, x) :: indexed (k + 1) xs

Displayable Question where
  display (Grade question bounds _)          = question ++ " " ++ show bounds 
  display (Open question _)                  = question 
  display (QCM question qcmOptions expected) = question ++ "\n" ++ enumerateOptions
    where 
      asString : (Nat, String) -> String
      asString (k, q) = show k ++ "- "  ++ q ++ "\n"
      
      enumerateOptions : String
      enumerateOptions = concatMap asString (indexed 1 qcmOptions)
      
data Answer : (q : Question) -> Type where
  AnswerQCM   : (option : Fin n) -> Answer (QCM {numOptions = n } q opts exp)
  AnswerGrade : (answer : Nat) -> {auto lbok : LTE lb answer} -> {auto ubok : LTE answer ub} ->  Answer (Grade q (lb, ub) exp)
  AnswerOpen  : (answer : String) -> Answer (Open q exp)
   
total 
isCorrectAnswer : (q : Question ) -> Answer q -> Bool
isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM   option) = option == expected
isCorrectAnswer (Grade question _ expected)                     (AnswerGrade answer) = answer == expected
isCorrectAnswer (Open question expected)                        (AnswerOpen  answer) = answer == expected

-- an impossible value which we use to construct contradictory proofs
VOID : _|_
VOID = hd []
where
  hd : List a -> a
  hd (x :: xs) = x
  
notANumber : Answer q -> Void
notANumber _ = VOID

tooLargeOption : Answer (QCM question qcmOptions expected) -> Void
tooLargeOption _ = VOID 

tooLarge : (contra : LTE num ub -> Void) -> Answer (Grade question (lb, ub) expected) -> Void
tooLarge contra (AnswerGrade {ubok} answer) = VOID

tooSmall : (contra : LTE lb num -> Void) -> Answer (Grade question (lb, ub) expected) -> Void
tooSmall contra x = VOID

validateAnswer : (s : String) -> (q : Question) -> Dec (Answer q)
validateAnswer s (QCM {numOptions} question qcmOptions expected) = 
  case parsePositive s of
    Nothing => No notANumber
    (Just n) => case integerToFin n numOptions of
                     Nothing => No tooLargeOption
                     (Just m) => Yes (AnswerQCM m)
validateAnswer s (Open question expected) = Yes (AnswerOpen s)
validateAnswer s (Grade question (lb, ub) expected) = 
  case parsePositive s of
    Nothing => No notANumber
    (Just n) => let num = (fromInteger n)
                in case isLTE lb num of
                     (Yes prf) => case isLTE num ub of
                                      (Yes prf) => Yes (AnswerGrade num) 
                                      (No contra) => No (tooLarge contra)
                     (No contra) => No (tooSmall contra)

readAnswer : (q : Question) -> IO (Answer q)
readAnswer q = do
  putStr (display q)
  input <- getLine
  case validateAnswer input q of 
    (Yes prf) => pure prf
    (No contra) => do
      putStr "Incorrect answer !"
      readAnswer q

main : IO () 
main = do
  let q = QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 2
  a <- readAnswer q
  if isCorrectAnswer q a
  then putStr "That's correct!"
  else putStr "Try again!"
