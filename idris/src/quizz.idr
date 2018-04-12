module quizz

import Data.String
import Data.Vect
import Data.Fin

%access public export

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



||| Magic values to simplify proofs
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

tooSmall : (contra : LTE num ub ->  Void) -> (a -> Void)
tooSmall contra _ = VOID

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
    (Just n) => let num = fromInteger n
                in case isLTE lb num of
                     (Yes prf) => case isLTE num ub of
                                      (Yes prf) => Yes (AnswerGrade num)
                                      (No contra) => No $ tooLarge contra
                     (No contra) => No $ tooSmall contra

readAnswer : (q : Question) -> IO (Answer q)
readAnswer q = do
  input <- getLine
  case validateAnswer input q of
    (Yes prf) => pure prf
    (No contra) => do
      putStr "Invalid answer !"
      readAnswer q

data Answered : Type where
  MkAnswered : (question ** Answer question) -> Answered

data Quizz : (numQuestions : Nat) -> Type where
  MkQuizz :  (answered : Vect n Answered) ->
             (current  : Question) ->
             (next : Vect m Question) ->
             Quizz (n + m)

data Input : Type where
  GoBack     : Input
  QuitGame   : Input
  GiveAnswer : String -> Input
  Garbage    : Input

data Command : Type -> Type where
  Prompt         : Question -> Command Input
  AnswerQuestion : String -> Command Bool
  Back           : Command ()
  Quit           : Command ()

total
plusOneCommutes : (n : Nat) -> (m : Nat) -> (n + S m = S n + m)
plusOneCommutes Z     k     = Refl
plusOneCommutes (S k) j     =
 let inductiveHypothesis = plusOneCommutes k j in
    rewrite inductiveHypothesis in Refl

updateQuizz : (current : Question)
           -> (q : Question)
           -> (qs : Vect len Question)
           -> (answered : Vect n Answered)
           -> (a : Answer current)
            -> Quizz (plus n (S len))
updateQuizz {n} {len} current q qs answered a =
  rewrite plusOneCommutes n len in
    MkQuizz (MkAnswered (current ** a) :: answered) q qs

goBack : (x : Question) -> (current : Question) -> (next : Vect m Question) -> (answered : Vect len Answered)
       -> Quizz (S (plus len m))
goBack {m} {len} x current next answered =
  rewrite plusSuccRightSucc len m in
    MkQuizz answered x (current :: next)


parseInput : (l : List Char) -> Input
parseInput []    = Garbage
parseInput ['q'] = QuitGame
parseInput ['b'] = GoBack
parseInput s     = GiveAnswer (pack s)

runCommand : Quizz n -> Command a -> IO (a, Quizz n)
runCommand quizz  (Prompt q)         = do
  putStrLn $ display q ++ " "
  l <- getLine
  pure (parseInput $ unpack l, quizz)

runCommand q@(MkQuizz answered current next) (AnswerQuestion x) = do
  a <- readAnswer current
  if isCorrectAnswer current a
  then do
    putStr "Correct !"
    case next of
      []        => pure (True, q)
      (x :: xs) => pure (True, updateQuizz current x xs answered a)
  else do
    putStrLn "That's Wrong !"
    pure (False, q)
runCommand q@(MkQuizz answered current next) Back  = do
  case answered of
    []                          => pure ((), q)
    (MkAnswered (x ** _) :: xs) => pure ((), goBack x current next xs)
runCommand q Quit    = pure ((), q)


sampleQuizz : Quizz 4
sampleQuizz = MkQuizz [] (Open "What is your name ?" "Sir Arthur")
              [ Open "What is your quest?" "To seek the holy grail"
              , QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 0
              , QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] 1
              , Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) 35
              ]


runQuizz : Quizz n -> IO ()
runQuizz quizz@(MkQuizz answered current next) = do
  (input, quizz') <- runCommand quizz (Prompt current)
  case input of
    GoBack => do
      ((), q') <- runCommand quizz' Back
      runQuizz q'
    QuitGame => pure ()
    (GiveAnswer x) => do
      (res, q') <- runCommand quizz' (AnswerQuestion x)
      runQuizz q'
    Garbage => do
      runQuizz quizz'
