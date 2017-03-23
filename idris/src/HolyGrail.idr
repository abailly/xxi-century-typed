module HolyGrail

import Data.String 
import Data.Vect
import Data.Fin

interface Displayable d where
  total display : d -> String
  
data Question : Type where
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
  display (Grade question bounds _) =
    question ++ " " ++ show bounds 
  display (QCM question qcmOptions expected) = 
    question ++ "\n" ++ enumerateOptions
    where 
      asString : (Nat, String) -> String
      asString (k, q) = show k ++ "- "  ++ q ++ "\n"
      
      enumerateOptions : String
      enumerateOptions = concatMap asString (indexed 1 qcmOptions)
      
data Answer : (q : Question) -> Type where
   AnswerQCM   : (option : Fin n) -> Answer (QCM {numOptions = n } q opts exp)
   AnswerGrade : (answer : Nat) -> {lbok : LTE lb answer} -> {ubok : LTE answer ub} ->  Answer (Grade q (lb, ub) exp)

total 
isCorrectAnswer : (q : Question ) -> Answer q -> Bool
isCorrectAnswer (QCM {numOptions} question qcmOptions expected) (AnswerQCM option) = option == expected
isCorrectAnswer (Grade question _ expected)                     (AnswerGrade answer) = answer == expected

-- an impossible value which we use to construct contradictory proofs
VOID : _|_
VOID = hd []
where
  hd : List a -> a
  hd (x :: xs) = x
  
notANumber : Answer (QCM question qcmOptions expected) -> Void
notANumber _ = VOID

tooLargeOption : Answer (QCM question qcmOptions expected) -> Void
tooLargeOption _ = VOID 

validateAnswer : (s : String) -> (q : Question) -> Dec (Answer q)
validateAnswer s (QCM {numOptions} question qcmOptions expected) = 
  case parsePositive s of
    Nothing => No notANumber
    (Just n) => case integerToFin n numOptions of
                     Nothing => No tooLargeOption
                     (Just m) => Yes (AnswerQCM m)

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
